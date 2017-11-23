{-# LANGUAGE Arrows #-}

module FRP.Ball where

import Config
import Debug.Trace
import Control.Lens (set)
import FRP.Yampa
import FRP.Yampa.VectorSpace
import Graphics.Renderer
import Graphics.Rendering.OpenGL (Color3(Color3), GLfloat)
import Linear(V2(..),translation,V3(..))
import Types

type BallIn = (Position, Maybe (V2 GLfloat), Maybe (V2 GLfloat), Bool)

type BallOut = (RenderInfo, V2 GLfloat)

reflect :: V2 GLfloat -> V2 GLfloat -> V2 GLfloat
reflect d n = d ^-^ (2*(d `dot` n)*^  n)

staticBallObject ::  SF BallIn BallOut
staticBallObject =
  let scaleMtx = scale (ballRadius*2) (ballRadius*2) 1
  in proc (V2 xP yP,_,_,_) ->
  do let renderInfo
           = (set translation (V3 xP (yP + 10 + ballRadius) 0) scaleMtx, 0,
              Color3 1 1 1)
     returnA -< (renderInfo, V2 xP (yP + 10 + ballRadius))


ballObject = dSwitch (staticBallObject &&& ballSense) flyingBallObject



ballSense :: SF BallIn (Event (V2 GLfloat,V2 GLfloat))
ballSense = proc (pos , maybeDir,_,_) -> do
  let ret = case maybeDir of
        Nothing -> NoEvent
        Just dir -> Event (pos,dir)
  returnA -< ret



flyingBallSense (_,_,_,q) =
  case q of
    True -> Event undefined
    False -> NoEvent

-- switchFunc (Left x) = flyingBallObject x
-- switchFunc (Right _) = staticBallObject

flyingBallObject :: (V2 GLfloat,V2 GLfloat) -> SF BallIn BallOut
flyingBallObject ((V2 x y),vel) =
  let scaleMtx = scale (ballRadius*2) (ballRadius*2) 1      
      addOffSet (V2 xPos yPos)  = V2 (xPos + x) (yPos+y + 10 + ballRadius)
      sig = proc ((_,_,maybeNormal,_),v) -> do
        let vNew = case maybeNormal of
                     Nothing -> v
                     Just normal -> if v `dot` normal < 0
                                       then reflect v normal
                                       else v
        rec
          (V2 xPos yPos,v') <- arr addOffSet *** identity <<< ((integral <<< arr (ballSpeed*^)) &&& identity) <<< arr clampedBallV -< ((V2 xPos yPos),vNew)
        let renderInfo = (set translation (V3 xPos yPos 0) scaleMtx,0,Color3 1 1 1)
        returnA -< ((renderInfo,V2 xPos yPos),v')
  in dSwitch (loopPre vel sig &&& arr flyingBallSense) (const ballObject)







clampedBallV (pos@(V2 x y),vel) =
  let leftNormal = V2 1 0
      rightNormal = V2 (-1) 0
      topNormal = V2 0 (-1)
      halfScreenWidth = fromIntegral screenWidth / 2
      halfScreenHeight = fromIntegral screenHeight / 2
  in if x-ballRadius <= -halfScreenWidth
     then if (vel `dot` leftNormal) < 0 then  (reflect vel leftNormal) else vel
     else if x+ballRadius >= halfScreenWidth
          then if (vel `dot` rightNormal) < 0  then reflect vel rightNormal else vel
          else if y+ballRadius>=halfScreenHeight
               then if (vel `dot` topNormal) <0 then reflect vel topNormal else vel
               else vel

