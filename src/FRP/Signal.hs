{-# LANGUAGE Arrows #-}
-- The main signal
module FRP.Signal where
import FRP.Yampa
import Data.Time.Clock
import Linear.Matrix
import Data.IORef
import SDL hiding (clear)
import Graphics.Rendering.OpenGL
import Graphics.Renderer
-- Return type of arrow
data Flow = Quit | Cont
data Info = Info Flow RenderInfo
type RenderInfo = [(M44 GLfloat,GLuint,Color3 GLfloat )]
sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  let dt = realToFrac $ now `diffUTCTime` lastTime
  pure (dt, Just $ if quit
                      then Quit
                      else Cont)


actuate vao program colorU modelU imageU window _ (Info Quit _)  = return True
actuate vao program colorU modelU imageU window _ (Info Cont r)  = do
  clear [ColorBuffer]
  renderNaive vao  program r colorU modelU imageU
  -- drawArrays Triangles 0 6
  glSwapWindow window
  traverse (putStrLn . show) <$> (get errors) 
  return False

sig = proc flow -> do
  let renderInfo = [(Graphics.Renderer.scale 100 100 100,0,Color3 0 1 0),((mkTransformationMat Linear.Matrix.identity (V3 200 200 0))!*!(Graphics.Renderer.scale 200 200 1), 0,Color3 1 0 0)]
  returnA -< Info flow renderInfo

initInput = return  Cont


runArrow window = do
  vao <- initVao
  (colorU,modelU,imageU, program) <-initResource

  timeRef <- newIORef =<< getCurrentTime
  reactimate initInput (sense timeRef) (actuate vao program colorU modelU imageU window) sig
