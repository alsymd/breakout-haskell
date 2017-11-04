{-# LANGUAGE Arrows #-}
-- The main signal
module FRP.Signal where
import FRP.Yampa
import GHC.Float
import Data.Time.Clock
import Linear.Matrix
import Data.IORef
import Control.Lens (set)
import SDL hiding (clear,time)
import Graphics.Rendering.OpenGL
import Graphics.Renderer
import Config
-- Return type of arrow
data Flow = Quit | Cont
data Info = Info Flow [RenderInfo]

sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  let dt = realToFrac $ now `diffUTCTime` lastTime
  pure (dt, Just $ if quit
                      then Quit
                      else Cont)


actuate vao program vbo window _ (Info Quit _)  = return True
actuate vao program vbo window _ (Info Cont r)  = do
  clear [ColorBuffer]
  renderNaive vao vbo program r
  glSwapWindow window
  traverse (putStrLn . show) <$> (get errors) 
  return False

xys =
  let halfWidth = fromIntegral screenWidth  /2 
      halfHeight = fromIntegral screenHeight /2
  in [let c = ((sin (-x-y)+1)/2) in (x,y,c) | x<-takeWhile (<halfWidth) $ [-halfWidth+25,-halfWidth+75..], y<-takeWhile  (<halfHeight) $[-halfHeight+25,-halfHeight+75..]]

sig = proc flow -> do
  t <- arr double2Float <<< arr (*2) <<< time -< undefined
  let renderInfo = [(Graphics.Renderer.scale 100 100 100,0,Color3 0 1 0),((mkTransformationMat Linear.Matrix.identity (V3 200 200 0))!*!(Graphics.Renderer.scale 200 200 1), 0,Color3 ((sin t + 1)/2) 1 1)]
      -- renderInfo' = fmap (\(x,y,c) -> ((mkTransformationMat Linear.Matrix.identity (V3 x y 0))!*! (Graphics.Renderer.scale 50 50 1),1,Color3 0 ((cos t+1)/2 * c) 0)) xys
  objInfos <- staticObjs -< undefined
  returnA -< Info flow ((Graphics.Renderer.scale 1024 768 1,2,Color3 1 1 1) : objInfos)

initInput = return  Cont


staticRenderable w h x y t r g b = constant (set  translation (V3 x y 1) (Graphics.Renderer.scale w h 1), t, Color3 r g b)

staticObjs = parB [staticRenderable 200 100 50 50 1 (240/255) (175/255) (36/255), staticRenderable 200 100 254 50 1 (240/255) (175/255) (36/255)]

runArrow window = do
  (vao,vbo,vbo1) <- initVao
  program <-initResource

  timeRef <- newIORef =<< getCurrentTime
  reactimate initInput (sense timeRef) (actuate vao program vbo1 window) sig

