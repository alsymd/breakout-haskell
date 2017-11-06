{-# LANGUAGE Arrows #-}
module FRP.Paddle where

import FRP.Yampa
import Graphics.Renderer
import Control.Lens(set)
import Types
import Graphics.Rendering.OpenGL(Color3(Color3),GLfloat)
import Config
import Linear


type PaddleIn = GameInput
type PaddleOut = (RenderInfo, V2 GLfloat)
type Velocity = GLfloat




paddleObject :: Position -> Size -> Velocity -> SF PaddleIn PaddleOut
paddleObject (V2 x y) (V2 w h) v =
  let scaleMtx = scale w h 1
      clamp = \ x v -> clampV x v w
  in
    proc gi -> do
    let vel = case lr gi of
              Just x -> if x == GoLeft then -v else v
              Nothing -> 0
    rec
      xPos <- integral -< (clamp xPos vel :: GLfloat)
    let renderInfo = (set translation (V3 xPos y 0 :: V3 GLfloat) scaleMtx, 4, Color3 1 1 1)
    returnA -< (renderInfo,V2 xPos y)

clampV :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clampV x v w = if x <= - fromIntegral screenWidth / 2 + w / 2
               then if v > 0 then v else 0
               else if x >= fromIntegral screenWidth / 2 - w /2
                    then if v < 0 then v else 0
                    else v

