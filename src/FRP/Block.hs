{-# LANGUAGE Arrows #-}
module FRP.Block where
import FRP.Yampa
import Data.Maybe
import Graphics.Renderer
import Graphics.Rendering.OpenGL (GLfloat, Color3(Color3))
import Linear
import Control.Lens(set)
import Config
import Types

leftNormal = V2 1 0
rightNormal = V2 (-1) 0
topNormal = V2 0 (-1)
bottomNormal = V2 0 1


type BlockIn = (V2 GLfloat)

data BlockType = Solid | Orange | Green | Cyan | White deriving(Show,Eq)

laifuSig = proc (evt, rem) -> do
         returnA -< if isEvent evt
                       then (rem-1,rem-1)
                       else (rem,rem)

laifuLoop = loopPre 0 laifuSig


colorSwitch c@(Color3 r g b)  = let init = constant c
                                    newColor = constant (Color3 (1-r) (1-g) (1-b))

                                in switch (init &&& edge) (const $ switch (newColor &&& edge <<< arr not) (const (colorSwitch c)))


data BlockOut = BlockOut
  {renderInfo :: RenderInfo
  ,dead :: Bool
  ,genPowerOff :: Bool
  ,collision :: Maybe (V2 GLfloat)}
   
-- No, you shouldn't arrest a function that kills a block. Unless it is an AI block and has AI rights.
callKiller :: SF (BlockIn, [BlockOut]) (Event [Bool])
callKiller = proc (_, blockOuts) -> do
                let killList = fmap dead blockOuts
                returnA -< case any (==False) killList of
                             True -> Event killList
                             _ -> noEvent

blockSerialKiller :: [SF BlockIn BlockOut] -> [Bool] -> SF BlockIn [BlockOut]
blockSerialKiller sigs killList = 
  let zipped = zip killList sigs
      newSigs = snd.unzip.filter ((==False) .fst) $ zipped
  in dpSwitchB newSigs (callKiller >>> notYet) blockSerialKiller


clamp x xmin xmax
      | x < xmin = xmin
      | x > xmax = xmax
      | otherwise = x

blockObject ::  Position -> Size -> BlockType -> SF BlockIn BlockOut
blockObject (V2 x y) (V2 w h) blockType =
  let mtx = set translation (V3 x y 0) (scale w h 1) 
      (destructible, color, texture) =
        case blockType of
          Solid -> (False, Color3 0.7 0.7 0.7, 3)
          Orange -> (True, Color3 1 0.5 0.0, 2)
          Green -> (True, Color3 0 0.7 0, 2)
          Cyan -> (True, Color3 0.2 0.6 1, 2)
          White -> (True, Color3 0.8 0.8 0.4, 2)
      cs = colorSwitch color
  in proc (V2 ballX ballY) -> do
          let xDiff = x - ballX
              yDiff = y - ballY
              halfWidth = w/2 + ballRadius
              halfHeight =h/2 + ballRadius
              maybeNormal = if abs xDiff > halfWidth || abs yDiff > halfHeight
                               then Nothing
                               else let vec1 = V2 xDiff yDiff
                                        vec2 = V2 (clamp (-xDiff) (-w/2) (w/2)) (clamp (-yDiff) (-h/2) (h/2))
                                        vec3 = vec1 + vec2
                                    in if vec3 `FRP.Yampa.dot` vec3 < ballRadius *ballRadius
                                          then Just $ FRP.Yampa.normalize vec3
                                          else Nothing

          c <- cs -< isJust maybeNormal
          let renderInfo = (mtx,texture,c)
          l <-laifuLoop <<< edge -< isJust maybeNormal
          returnA -< BlockOut {renderInfo = renderInfo,  dead = if blockType /= Solid then l < 0 else False, genPowerOff = False, collision = fmap negate maybeNormal}
