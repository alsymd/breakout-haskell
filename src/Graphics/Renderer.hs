module Graphics.Renderer where
import Data.Vector.Storable
import Data.GameResource
import Graphics.Rendering.OpenGL 
import Prelude hiding (length,head)
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Linear
import Linear.Projection
import Config


projection :: M44 GLfloat
projection = Linear.Projection.ortho (-halfWidth) halfWidth (halfHeight) (-halfHeight) (-1) 1
           where halfWidth = fromIntegral screenWidth / 2
                 halfHeight = fromIntegral screenHeight / 2

bufferDataWithVector ::
     (Storable a) => Vector a -> BufferTarget -> BufferUsage -> IO ()
bufferDataWithVector v target usage =
  unsafeWith v $ \ptr ->
    bufferData target $=
    ( fromIntegral $ length v * sizeOf (undefined `asTypeOf` head v)
    , castPtr ptr
    , usage)


scale :: (Num a) => a -> a-> a -> M44 a
scale sx sy sz =
  V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)

initVao :: IO VertexArrayObject
initVao = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  bufferDataWithVector vertices ArrayBuffer StaticDraw
  vertexAttribPointer (AttribLocation 0) $=
    (ToFloat,
     VertexArrayDescriptor
      4
      Float
      (fromIntegral $ 4* sizeOf (undefined ::GLfloat))
      (intPtrToPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  bindVertexArrayObject $= Nothing
  pure vao
  where vertices :: Vector GLfloat
        vertices = fromList [-0.5,0.5,0.0,1.0
                            ,-0.5,-0.5,0.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,0.5,1.0,1.0
                            ,-0.5,0.5,0.0,1.0]


initResource :: IO (Int->Color3 GLfloat -> IO (), Int -> GLmatrix GLfloat -> IO (), Int -> GLuint -> IO (), Program)
initResource = do
  Right resource <- loadResourceFromFiles "awesomeface.png" "awesomeface.png" "awesomeface.png" "shader.vert" "shader.frag"
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just (ballTexture resource)
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just (backgroundTexture resource)
  activeTexture $= TextureUnit 2
  textureBinding Texture2D $= Just (brickTexture resource)
  let program = shaderProgram resource
  currentProgram $= Just program
  mtx <- toGLmatrix projection :: IO (GLmatrix GLfloat)
  join $ uniformFunc program "image[0]" <*> (pure.TextureUnit) 0
  join $ uniformFunc program "image[1]" <*> (pure.TextureUnit) 1
  join $ uniformFunc program "image[2]" <*> (pure.TextureUnit) 2
  join $ uniformFunc program "projection" <*> pure mtx
  return (uniformFuncArray program "spriteColor",uniformFuncArray program "model",uniformFuncArray program "image_idx",program)

uniformFunc :: Uniform a => Program -> String -> IO (a -> IO ())
uniformFunc program uStr = do
  uniformLoc <- get $ uniformLocation program uStr
  pure (\x -> uniform uniformLoc $= x)

uniformFuncArray :: Uniform a => Program -> String ->  (Int -> a -> IO ())
uniformFuncArray program uStr idx datum = do
  uniformLoc <- get $ uniformLocation program (uStr L.++ "[" L.++ show idx L.++ "]")
  uniform uniformLoc $= datum

renderNaive :: VertexArrayObject -> Program -> [(M44 GLfloat,GLuint,Color3 GLfloat )] -> (Int->Color3 GLfloat -> IO ()) -> (Int->GLmatrix GLfloat -> IO ()) -> (Int->GLuint -> IO ()) -> IO ()
renderNaive vao program xs colorU modelU imageU =
  let l = L.genericLength xs
      toAction i (m,idx,c) =
        do colorU i c
           glm <- toGLmatrix m
           modelU i glm
           imageU i idx
  in do currentProgram $= Just program
        sequence_ $ fmap (uncurry toAction) $ L.zip [0..] xs
        bindVertexArrayObject $= Just vao
        drawArraysInstanced Triangles 0 6 l

toGLmatrix :: (Matrix m, MatrixComponent c) => M44 c -> IO (m c)
toGLmatrix mr =
  let mc = transpose mr
  in withNewMatrix ColumnMajor (flip poke mc . castPtr)
