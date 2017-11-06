{-# LANGUAGE Arrows #-}

module FRP.Ball where

import Config
import Control.Lens (set)
import FRP.Yampa
import Graphics.Renderer
import Graphics.Rendering.OpenGL (Color3(Color3), GLfloat)
import Linear
import Types

type BallIn = (Position, Maybe (V2 GLfloat))

type BallOut = RenderInfo

staticBallObject ::  SF BallIn BallOut
staticBallObject =
  let scaleMtx = scale ballRadius ballRadius 1
  in proc (V2 xP yP,_) ->
  do let renderInfo
           = (set translation (V3 xP (yP + 10 + ballRadius / 2) 0) scaleMtx, 0,
              Color3 1 1 1)
     returnA -< renderInfo


ballObject = switch (staticBallObject &&& ballSense) flyingBallObject


ballSense :: SF BallIn (Event (V2 GLfloat,V2 GLfloat))
ballSense = proc (pos , maybeDir) -> do
                 let ret = case maybeDir of
                             Nothing -> noEvent
                             Just dir -> Event (pos,dir)
                 returnA -< ret


flyingBallObject ((V2 x y),(V2 dirX dirY)) =
  let scaleMtx = scale ballRadius ballRadius 1
  in proc _ -> do
  xPos <- arr (+x) <<< integral -< ballSpeed*dirX
  yPos <- arr (+ (y+10+ballRadius/2)) <<< integral -< ballSpeed*dirY
  let renderInfo = (set translation (V3 xPos yPos 0) scaleMtx,0,Color3 1 1 1)
  returnA -< renderInfo
