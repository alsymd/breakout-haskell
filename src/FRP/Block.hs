{-# LANGUAGE Arrows #-}
module FRP.Block where
import FRP.Yampa
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

data BlockType = Solid | Orange | Green | Cyan | White

laifuSig = proc (evt, rem) -> do
         returnA -< if isEvent evt
                       then (rem-1,rem-1)
                       else (rem,rem)

laifuLoop = loopPre 1 laifuSig


colorSwitch c@(Color3 r g b)  = let init = constant c
                                    newColor = constant (Color3 (1-r) (1-g) (1-b))

                                in switch (init &&& edge) (const $ switch (newColor &&& edge <<< arr not) (const (colorSwitch c)))


data BlockOut = BlockOut
  {renderInfo :: RenderInfo
  ,dead :: Bool
  ,genPowerOff :: Bool
  ,collision :: Maybe (V2 GLfloat)}
-- constant BlockOut{renderInfo = renderInfo, dead = False, genPowerOff = False}
   
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
      hitreact = undefined
      -- renderInfo = (mtx,texture,color) :: RenderInfo
      cs = colorSwitch color
  in proc (V2 ballX ballY) -> do
          let xDiff = x - ballX
              yDiff = y - ballY
              halfWidth = (w + ballRadius)/2
              halfHeight =(h + ballRadius)/2
              hasCollision = abs xDiff <= halfWidth && abs yDiff <= halfHeight
          c <- cs -< hasCollision
          let renderInfo = (mtx,texture,c)
          l <-laifuLoop <<< edge -< hasCollision
          returnA -< BlockOut {renderInfo = renderInfo, dead = l < 0, genPowerOff = False, collision = if hasCollision then (Just (V2 0 (-1))) else Nothing}
