module Types where
import Linear(V2)
import Graphics.Rendering.OpenGL(GLfloat)
type Position = V2 GLfloat
type Size = V2 GLfloat

data GameInput = GoLeft | GoRight deriving (Show,Eq)
