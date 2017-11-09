{-# LANGUAGE Arrows #-}
module FRP.Paddle where

import FRP.Yampa
import Graphics.Renderer
import Control.Lens(set)
import Types
import Graphics.Rendering.OpenGL(Color3(Color3),GLfloat)
import Config
import Linear


type PaddleIn = (GameInput, V2 GLfloat)
type PaddleOut = (RenderInfo, V2 GLfloat, Maybe (V2 GLfloat))
type Velocity = GLfloat




paddleObject :: Position -> Size -> Velocity -> SF PaddleIn PaddleOut
paddleObject (V2 x y) (V2 w h) v =
  let scaleMtx = scale w h 1
      clamp = \ x v -> clampV x v w
      (V2 paddleWidth paddleHeight) = paddleSize
  in
    proc (gi,~(V2 ballX ballY)) -> do
    let vel = case lr gi of
              Just x -> if x == GoLeft then -v else v
              Nothing -> 0
    rec
      xPos <- integral -< (clamp xPos vel :: GLfloat)
    let xDiff =  xPos - ballX
        yDiff = y - ballY
        halfWidth = (ballRadius + paddleWidth)/2
        halfHeight = (ballRadius + paddleHeight)/2

    let renderInfo = (set translation (V3 xPos y 0 :: V3 GLfloat) scaleMtx, 4, Color3 1 1 1)
    returnA -< (renderInfo,V2 xPos y, if abs xDiff <= halfWidth && abs yDiff <= halfHeight
                                         then Just (V2 0 1)
                                         else Nothing)

clampV :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clampV x v w = if x <= - fromIntegral screenWidth / 2 + w / 2
               then if v > 0 then v else 0
               else if x >= fromIntegral screenWidth / 2 - w /2
                    then if v < 0 then v else 0
                    else v

