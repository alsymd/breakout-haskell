{-# LANGUAGE Arrows #-}
module FRP.Block where
import FRP.Yampa
import Graphics.Renderer
import Graphics.Rendering.OpenGL (GLfloat, Color3(Color3))
import Linear
import Control.Lens(set)
import Types



type BlockIn = Position

data BlockType = Solid | Orange | Green | Cyan | White

data BlockOut = BlockOut
  {renderInfo :: RenderInfo
  ,dead :: Bool
  ,genPowerOff :: Bool}

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
      renderInfo = (mtx,texture,color) :: RenderInfo
  in constant BlockOut{renderInfo = renderInfo, dead = False, genPowerOff = False}
