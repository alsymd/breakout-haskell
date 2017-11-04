{-# LANGUAGE OverloadedStrings #-}
module Config where

import SDL hiding (Vector)
import Graphics.Rendering.OpenGL



-- Top Level Configurations
screenWidth = 1024
screenHeight = 768
maxInstance = 2000
title = "Breakout"
profile = Core Normal 4 4


glConfig = defaultOpenGL {glProfile = profile, glMultisampleSamples = 8}


windowConfig = defaultWindow
  {windowOpenGL = Just glConfig
  ,windowInputGrabbed = False
  ,windowInitialSize = V2 screenWidth screenHeight}

initializeSDL :: IO Window
initializeSDL = do
  initialize [InitVideo, InitEvents]
  -- setMouseLocationMode RelativeLocation
  window <- createWindow title windowConfig
  glCreateContext window
  clearColor $= Color4 (102/255) (204/255) (255/255) 1
  cullFace $= Just Back
  return window


twoColor = (1,0,0)
threeColor = (0,1,0)
fourColor = (0,0,1)
