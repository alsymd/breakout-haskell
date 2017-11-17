{-# LANGUAGE Arrows #-}
module FRP.Particle where

import FRP.Yampa
import Data.Monoid
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
        alpha = double2Float $ 1 - t/laifu
    returnA -< (state, (offset, Color4 1 1 1 alpha))

trackBallVel :: V2 GLfloat -> SF (V2 GLfloat) (Maybe (V2 GLfloat))
trackBallVel = flip loopPre $ 
  proc (currPos, prevPos) -> do
    let offset = currPos Linear.^-^ prevPos
    returnA -< (if offset == zeroVector then Nothing else Just $ offset,currPos)
  

callSpawner :: V2 GLfloat -> SF ((V2 GLfloat, (V2 GLfloat,V2 GLfloat)) , [ParticleOut]) (Event (Maybe (V2 GLfloat), Maybe [ParticleState], (V2 GLfloat, V2 GLfloat), V2 GLfloat))
callSpawner initBallPos =
  let offsetSig = trackBallVel initBallPos
  in proc ((ballPos, randomOffsets) ,particleOuts) -> do
    maybeVel <- offsetSig -< ballPos
    let states = fmap fst particleOuts
        maybeStates = if any (==Dead) states then Just states else Nothing
    returnA -< if isJust maybeStates || isJust maybeVel then Event (maybeVel,maybeStates, randomOffsets, ballPos) else noEvent


particleSpawner :: [SF (V2 GLfloat, (V2 GLfloat, V2 GLfloat)) ParticleOut] -> (Maybe (V2 GLfloat), Maybe [ParticleState], (V2 GLfloat,V2 GLfloat), V2 GLfloat) -> SF (V2 GLfloat, (V2 GLfloat, V2 GLfloat))[ParticleOut]
particleSpawner sigs (maybeVel,maybeParticleState,randomOffsets,ballPos) =
  let newSigs = case maybeVel of
        Just vel -> genParticles randomOffsets ballPos vel
        Nothing -> []
  in case maybeParticleState of
       Just particleStates -> let aliveSigs = fst . unzip . filter ((==Alive).snd) $ zip sigs particleStates
                                  currSigs = newSigs <> aliveSigs
                              in dpSwitchB currSigs (callSpawner (V2 0 0) >>> notYet) particleSpawner
       Nothing -> dpSwitchB (newSigs <> sigs) (callSpawner (V2 0 0)  >>> notYet) particleSpawner
  where genParticles (offset1, offset2) pos vel = [particleObject (pos Linear.^+^ offset1) vel 0.5, particleObject (pos Linear.^+^ offset2) vel 0.5]
