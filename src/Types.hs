module Types where
import Linear(V2)
import Graphics.Rendering.OpenGL(GLfloat)
type Position = V2 GLfloat
type Size = V2 GLfloat

data GameInput = GameInput {lr :: Maybe Direction, shoot :: Maybe Shoot} deriving (Show,Eq)
data Direction = GoLeft | GoRight deriving (Show, Eq)
data Shoot = Shoot deriving (Show, Eq)
