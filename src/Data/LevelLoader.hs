module Data.LevelLoader where
import FRP.Block
import Graphics.Rendering.OpenGL(GLfloat)
import Config
import Control.Monad
import Linear(V2(V2))

int2BlockType 1 = Solid
int2BlockType 2 = Orange
int2BlockType 3 = Green
int2BlockType 4 = Cyan
int2BlockType 5 = White

fromListToBlock xs =
  let cols = length $ head xs
      rows = length $ xs
      bwidth = (fromIntegral screenWidth/) . fromIntegral $ cols :: GLfloat
      bheight = bwidth / 1.61803398875
      wBeg = - fromIntegral screenWidth / 2 + bwidth/2
      hBeg = bheight/2
      flattened = join xs
      poses = [(x,y) | y<-fmap ((fromIntegral screenHeight / 2) - ) . take rows$ [hBeg,hBeg+bheight ..], x <- take cols [wBeg, wBeg + bwidth ..]]
      f ((x,y),texture) = blockObject (V2 x y) (V2 bwidth bheight) (int2BlockType texture)
  in fmap f . filter ((/=0) . snd) $ zip poses flattened

          
loadLevelFromFile f = (fmap (fmap read).fmap words.lines<$>readFile f) >>= return . fromListToBlock
