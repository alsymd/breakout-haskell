{-# LANGUAGE Arrows #-}

module FRP.Ball where

import Config
import Control.Lens (set)
import FRP.Yampa
import FRP.Yampa.VectorSpace
import Graphics.Renderer
import Graphics.Rendering.OpenGL (Color3(Color3), GLfloat)
import Linear(V2(..),translation,V3(..))
import Types

type BallIn = (Position, Maybe (V2 GLfloat), Maybe (V2 GLfloat))

type BallOut = (RenderInfo, V2 GLfloat)

reflect :: V2 GLfloat -> V2 GLfloat -> V2 GLfloat
reflect d n = d ^-^ (2*(d `dot` n)*^  n)

staticBallObject ::  SF BallIn BallOut
staticBallObject =
  let scaleMtx = scale ballRadius ballRadius 1
  in proc (V2 xP yP,_,_) ->
  do let renderInfo
           = (set translation (V3 xP (yP + 10 + ballRadius / 2) 0) scaleMtx, 0,
              Color3 1 1 1)
     returnA -< (renderInfo, V2 xP (yP + 10 + ballRadius / 2))


ballObject = switch (staticBallObject &&& ballSense) flyingBallObject


ballSense :: SF BallIn (Event (V2 GLfloat,V2 GLfloat))
ballSense = proc (pos , maybeDir,_) -> do
                 let ret = case maybeDir of
                             Nothing -> noEvent
                             Just dir -> Event (pos,dir)
                 returnA -< ret


flyingBallObject ((V2 x y),vel) =
  let scaleMtx = scale ballRadius ballRadius 1      
      addOffSet (V2 xPos yPos)  = V2 (xPos + x) (yPos+y + 10 + ballRadius/2)
      sig = proc ((_,_,maybeNormal),v) -> do
        rec
          (V2 xPos yPos,v') <- (arr addOffSet *** identity) <<< ((integral <<< arr (ballSpeed*^)) &&& identity) -< clampedBallV (V2 xPos yPos) $ case maybeNormal of
                                                                                                                                                   Just normal -> reflect v normal
                                                                                                                                                   Nothing -> v
        let renderInfo = (set translation (V3 xPos yPos 0) scaleMtx,0,Color3 1 1 1)
        returnA -< ((renderInfo,V2 xPos yPos),v')
  in loopPre vel sig

clampedBallV pos@(V2 x y) vel =
  let leftNormal = (V2 1 0)
      rightNormal = (V2 (-1) 0)
      topNormal = (V2 0 (-1))
      halfScreenWidth = fromIntegral screenWidth / 2
      halfScreenHeight = fromIntegral screenHeight / 2
  in if x <= -halfScreenWidth
     then reflect vel leftNormal
     else if x >= halfScreenWidth
          then reflect vel rightNormal
          else if y>=halfScreenHeight
               then reflect vel topNormal
               else vel
    
