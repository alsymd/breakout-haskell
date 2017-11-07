{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where
import Linear(V2(..))
import Graphics.Rendering.OpenGL(GLfloat)
import FRP.Yampa.VectorSpace
type Position = V2 GLfloat
type Size = V2 GLfloat

data GameInput = GameInput {lr :: Maybe Direction, shoot :: Maybe Shoot} deriving (Show,Eq)
data Direction = GoLeft | GoRight deriving (Show, Eq)
data Shoot = Shoot deriving (Show, Eq)
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = V2 0 0
  a *^ (V2 x y) = V2 (a*x) (a*y)
  negateVector (V2 x y) = V2 (negate x) (negate y)
  (V2 x y) ^+^ (V2 x' y') = V2 (x+x') (y+y')
  (V2 x y) `dot` (V2 x' y') = x*x' + y *y'
