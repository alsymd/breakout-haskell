{-# LANGUAGE Arrows #-}
module FRP.Particle where

import FRP.Yampa
import Data.Maybe
import Graphics.Renderer
import GHC.Float
import Control.Lens(set)
import Types
import Graphics.Rendering.OpenGL(Color4(Color4),GLfloat)
import Config
import Linear

data ParticleState = Dead | Alive deriving(Show, Eq)

type ParticleOut = (ParticleState,ParticleRenderInfo)

particleObject :: V2 GLfloat -> V2 GLfloat -> Time -> SF a ParticleOut
particleObject pos vel laifu =
  let posSig = arr (Linear.^+^ pos) <<< integral
  in proc _ -> do
    offset <- posSig -< vel
    t <- time -< undefined
    let state = if t > laifu then Dead else Alive
        alpha = double2Float $ t/laifu
    returnA -< (state, (offset, Color4 1 1 1 alpha))

trackBallVel :: V2 GLfloat -> SF (V2 GLfloat) (Maybe (V2 GLfloat))
trackBallVel = flip loopPre $ 
  proc (prevPos, currPos) -> do
    let offset = currPos Linear.^-^ prevPos
    returnA -< (if offset == zeroVector then Nothing else Just $ FRP.Yampa.normalize offset,currPos)
  

callSpawner :: V2 GLfloat -> SF (V2 GLfloat, [ParticleOut]) (Event (Maybe (V2 GLfloat), Maybe [ParticleState]))
callSpawner initBallPos =
  let offsetSig = trackBallVel initBallPos
  in proc (ballPos, particleOuts) -> do
    maybeVel <- offsetSig -< ballPos
    let states = fmap fst particleOuts
        maybeStates = if any (==Dead) states then Just states else Nothing
    returnA -< if isJust maybeStates || isJust maybeVel then Event (maybeVel,maybeStates) else noEvent

