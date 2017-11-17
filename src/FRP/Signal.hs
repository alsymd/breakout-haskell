{-# LANGUAGE Arrows #-}
-- The main signal
module FRP.Signal where
import FRP.Yampa
import FRP.Particle
import System.Random
import Control.Monad
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

data Flow = Quit | Cont
data Info = Info Flow [RenderInfo] [ParticleRenderInfo]

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


actuate vao program vbo vao' program' vbo' window _ (Info Quit _ _)  = return True
actuate vao program vbo vao' program' vbo' window _ (Info Cont r p)  = do
  clear [ColorBuffer]
  renderNaive vao vbo program r
  renderParticle vao' vbo' program' p
  glSwapWindow window
  traverse (putStrLn . show) <$> (get errors) 
  return False


sigPaddle = paddleObject (V2 0 (-340)) paddleSize paddleVelocity
sigBall = ballObject 
sigParticles = dpSwitchB  [] (callSpawner (V2 0 0) >>> notYet) particleSpawner

sig seed blocks = let bsig = dpSwitchB blocks callKiller blockSerialKiller
                      (seed1P,seed2P) = split seed
                      (seed1,seed1') = split seed1P
                      (seed2,seed2') = split seed2P
                      noise1 = noiseR (-5,5) seed1
                      noise1' = noiseR (-5,5) seed1'
                      noise2 = noiseR (-5,5) seed2
                      noise2' = noiseR (-5,5) seed2'
                      
                      
  in
  proc (MainSigIn flow gi) -> do
  -- t <- arr double2Float <<< arr (*2) <<< time -< undefined
  
  rec
    (paddleR, pPos,mcol) <- sigPaddle -< (gi,ballPos)
    (ballR, ballPos) <- (sigBall) -< (pPos, if isJust (shoot gi)
                                               then Just (V2 0.707107 0.707107)
                                               else Nothing,   (\xs -> if null xs
                                                                          then Nothing
                                                                          else Just $ head xs) .catMaybes . (mcol:) .fmap collision $ objInfos)
    objInfos <- bsig -< ballPos
  
  offset1x <-noise1 -< undefined
  offset1y <-noise1' -< undefined
  offset2x <-noise2 -< undefined
  offset2y <-noise2' -< undefined
  let offsetV1 = V2 offset1x offset1y
      offsetV2 = V2 offset2x offset2y
  pOut<-sigParticles -< (ballPos,(offsetV1,offsetV2))
  returnA -< Info flow ((Graphics.Renderer.scale 1024 768 1,1,Color3 1 1 1):paddleR :  fmap renderInfo objInfos ++ [ballR]) (snd.unzip $ pOut)


initInput = return $ MainSigIn Cont (GameInput Nothing Nothing)

runArrow window = do
  (vao,vbo,vbo1) <- initVao
  (vao',vbo',vbo1') <- initParticleVao
  seed <- getStdGen
  (program,program') <-initResource
  level <- loadLevelFromFile "level2.dat"
  timeRef <- newIORef =<< getCurrentTime
  reactimate initInput (sense timeRef) (actuate vao program vbo1 vao' program' vbo1' window) (sig seed level)

