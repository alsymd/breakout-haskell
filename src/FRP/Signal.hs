{-# LANGUAGE Arrows #-}
-- The main signal
module FRP.Signal where
import FRP.Yampa
import FRP.Ball
import Data.Maybe
import Data.LevelLoader
import FRP.Block
import GHC.Float
import Data.Time.Clock
import Linear.Matrix
import Data.IORef
import Control.Lens (set)
import SDL hiding (clear,time)
import Graphics.Rendering.OpenGL
import Graphics.Renderer
import Config
import FRP.Paddle
import Types
-- Return type of arrow
data Flow = Quit | Cont
data Info = Info Flow [RenderInfo]

data MainSigIn = MainSigIn Flow GameInput


sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  keyState <- getKeyboardState
  let gi = if keyState ScancodeLeft then Just GoLeft
           else if keyState ScancodeRight then Just GoRight else Nothing
  let gi' = if keyState ScancodeSpace
               then Just Shoot
               else Nothing
  let dt = realToFrac $ now `diffUTCTime` lastTime
  let flow = if quit then Quit else Cont
  pure (dt, Just (MainSigIn flow (GameInput gi gi')))


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


sigPaddle = paddleObject (V2 0 (-340)) paddleSize paddleVelocity
sigBall = ballObject 

sig blocks = let bsig = parB blocks in
  proc (MainSigIn flow gi) -> do
  t <- arr double2Float <<< arr (*2) <<< time -< undefined
  objInfos <- bsig -< V2 1 2
  (paddleR, pPos) <- sigPaddle -< gi
  ballR <- sigBall -< (pPos, if isJust (shoot gi)
                                then Just (V2 0.707107 0.707107)
                                else Nothing)
  returnA -< Info flow ((Graphics.Renderer.scale 1024 768 1,1,Color3 1 1 1):paddleR :  fmap renderInfo objInfos ++ [ballR])

initInput = return $ MainSigIn Cont (GameInput Nothing Nothing)


staticRenderable w h x y t r g b = constant (set  translation (V3 x y 1) (Graphics.Renderer.scale w h 1), t, Color3 r g b)

staticObjs = parB [staticRenderable 200 100 50 50 1 0 0.3 0.9, staticRenderable 200 100 254 50 1 0 0.3 0.9]

runArrow window = do
  (vao,vbo,vbo1) <- initVao
  program <-initResource
  level <- loadLevelFromFile "level2.dat"
  timeRef <- newIORef =<< getCurrentTime
  reactimate initInput (sense timeRef) (actuate vao program vbo1 window) (sig level)

