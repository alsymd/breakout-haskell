{-# LANGUAGE Arrows #-}
module FRP.Particle where

import FRP.Yampa
import Graphics.Renderer
import Control.Lens(set)
import Types
import Graphics.Rendering.OpenGL(Color3(Color3),GLfloat)
import Config
import Linear

