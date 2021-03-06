module Graphics.Renderer where
import Data.Vector.Storable
import Data.GameResource
import Graphics.Rendering.OpenGL
import Data.Foldable (traverse_,toList,foldl')
import Prelude hiding (length,head)
import qualified Data.List as L
import System.IO (hPutStrLn,stderr)
import Control.Applicative
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Linear
import Graphics.GL.Core40
import Linear.Projection
import Config (screenWidth,screenHeight,maxInstance)

type RenderInfo = (M44 GLfloat,GLint,Color3 GLfloat )
type ParticleRenderInfo = (V2 GLfloat, Color4 GLfloat)


renderInfoSize = sizeOf (undefined::GLfloat) * 20
particleRenderInfoSize = sizeOf (undefined::GLfloat) * 6

projection :: M44 GLfloat
projection = Linear.Projection.ortho (-halfWidth) halfWidth (-halfHeight) (halfHeight) (-1) 1
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

initVao :: IO (VertexArrayObject,BufferObject,BufferObject)
initVao = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  vbo2 <- genObjectName

  bufferDataWithVector vertices ArrayBuffer StaticDraw
  vertexAttribPointer (AttribLocation 0) $=
    (ToFloat,
     VertexArrayDescriptor
      4
      Float
      (fromIntegral $ 4* sizeOf (undefined ::GLfloat))
      (intPtrToPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  bindBuffer ArrayBuffer $= Just vbo2

  -- Dirty code for setting up instanced array

  bufferData ArrayBuffer $=
   ( fromIntegral renderInfoSize * fromIntegral maxInstance
   , nullPtr
   , DynamicDraw)
  vertexAttribPointer (AttribLocation 1) $=
    (ToFloat,
     VertexArrayDescriptor 4 Float (fromIntegral renderInfoSize) (intPtrToPtr 0))
  vertexAttribArray (AttribLocation 1) $= Enabled
  
  vertexAttribPointer (AttribLocation 2) $=
    (ToFloat
    ,VertexArrayDescriptor 4 Float (fromIntegral renderInfoSize) (intPtrToPtr . fromIntegral $ 4 * sizeOf (undefined::GLfloat)))
  vertexAttribArray (AttribLocation 2) $= Enabled

  vertexAttribPointer (AttribLocation 3) $=
    (ToFloat
    ,VertexArrayDescriptor 4 Float (fromIntegral renderInfoSize) (intPtrToPtr . fromIntegral $ 8 * sizeOf (undefined::GLfloat)))
  vertexAttribArray (AttribLocation 3) $= Enabled

  
  vertexAttribPointer (AttribLocation 4) $=
    (ToFloat
    ,VertexArrayDescriptor 4 Float (fromIntegral renderInfoSize) (intPtrToPtr . fromIntegral $ 12 * sizeOf (undefined::GLfloat)))
  vertexAttribArray (AttribLocation 4) $= Enabled
  vertexAttribPointer (AttribLocation 5) $=
    (KeepIntegral
    ,VertexArrayDescriptor 1 Int (fromIntegral renderInfoSize) (intPtrToPtr . fromIntegral $ 16 * sizeOf (undefined :: GLfloat)))
  vertexAttribArray (AttribLocation 5) $= Enabled
  vertexAttribPointer (AttribLocation 6) $=
    (ToFloat
    ,VertexArrayDescriptor 3 Float (fromIntegral renderInfoSize) (intPtrToPtr . fromIntegral $ 17 * sizeOf (undefined :: GLfloat)))
  vertexAttribArray (AttribLocation 6) $= Enabled
  -- Danger! Raw GL Call!! 
  traverse_ (flip glVertexAttribDivisor 1) [1..6]

  

  bindVertexArrayObject $= Nothing
  pure (vao,vbo,vbo2)
  where vertices :: Vector GLfloat
        vertices = fromList [-0.5,0.5,0.0,1.0
                            ,-0.5,-0.5,0.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,0.5,1.0,1.0
                            ,-0.5,0.5,0.0,1.0]



initParticleVao :: IO (VertexArrayObject, BufferObject, BufferObject)
initParticleVao = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  vbo2 <- genObjectName
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
  bindBuffer ArrayBuffer $= Just vbo2

  bufferData ArrayBuffer $= ( fromIntegral particleRenderInfoSize * fromIntegral maxInstance
   , nullPtr
   , DynamicDraw)
  vertexAttribPointer (AttribLocation 1) $=
    (ToFloat,
     VertexArrayDescriptor 2 Float (fromIntegral particleRenderInfoSize) (intPtrToPtr 0))
  vertexAttribArray (AttribLocation 1) $= Enabled



  vertexAttribPointer (AttribLocation 2) $=
    (ToFloat
    ,VertexArrayDescriptor 4 Float (fromIntegral particleRenderInfoSize) (intPtrToPtr . fromIntegral $ 2 * sizeOf (undefined::GLfloat)))
  vertexAttribArray (AttribLocation 2) $= Enabled
  
  traverse_ (flip glVertexAttribDivisor 1) [1..2]
  pure (vao,vbo,vbo2)
  where vertices :: Vector GLfloat
        vertices = fromList [-0.5,0.5,0.0,1.0
                            ,-0.5,-0.5,0.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,-0.5,1.0,0.0
                            ,0.5,0.5,1.0,1.0
                            ,-0.5,0.5,0.0,1.0]


initResource :: IO (Program,Program)
initResource = do
  Right resource <- loadResourceFromFiles "awesomeface.png" "background.png"  "block.png" "block_solid.png" "paddle.png" "shader.vert" "shader.frag"  "particle.vert" "particle.frag" "particle.png"
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just (ballTexture resource)
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just (backgroundTexture resource)
  activeTexture $= TextureUnit 2
  textureBinding Texture2D $= Just (blockTexture resource)
  activeTexture $= TextureUnit 3
  textureBinding Texture2D $= Just (solidBlockTexture resource)
  activeTexture $= TextureUnit 4
  textureBinding Texture2D $= Just (paddleTexture resource)
  activeTexture $= TextureUnit 5
  textureBinding Texture2D $= Just (particleTexture resource)
  let program = shaderProgram resource
      program' = particleProgram resource
  currentProgram $= Just program
  mtx <- toGLmatrix projection :: IO (GLmatrix GLfloat)
  join $ uniformFunc program "image[0]" <*> (pure.TextureUnit) 0
  join $ uniformFunc program "image[1]" <*> (pure.TextureUnit) 1
  join $ uniformFunc program "image[2]" <*> (pure.TextureUnit) 2
  join $ uniformFunc program "image[3]" <*> (pure.TextureUnit) 3
  join $ uniformFunc program "image[4]" <*> (pure.TextureUnit) 4
  join $ uniformFunc program "projection" <*> pure mtx
  currentProgram $= Just program'
  join $ uniformFunc program' "image" <*> (pure.TextureUnit) 5
  join $ uniformFunc program' "projection" <*> pure mtx
  pure (program,program')






uniformFunc :: Uniform a => Program -> String -> IO (a -> IO ())
uniformFunc program uStr = do
  uniformLoc <- get $ uniformLocation program uStr
  pure (\x -> uniform uniformLoc $= x)

-- Yeah I made this generic
pokeRenderInfo infos ptr =
  let ptrs = L.scanl plusPtr ptr (repeat renderInfoSize) :: [Ptr GLfloat]
      xs = zip ptrs (Data.Foldable.toList infos) :: [(Ptr GLfloat, RenderInfo)]
  in  traverse_ f xs
  where f :: (Ptr GLfloat, RenderInfo) -> IO ()
        f (beg, (mtx,idx,color)) =
          let mtxBegin = beg :: Ptr GLfloat
              imageIdxBegin = beg `plusPtr`  (16*sizeOf (undefined::GLfloat))
              colorBegin = beg `plusPtr` (17*sizeOf (undefined::GLfloat))
          in do poke (castPtr mtxBegin) (transpose mtx)
                poke (castPtr imageIdxBegin) idx
                poke (castPtr colorBegin) color



pokeParticleRenderInfo infos ptr =
  let ptrs = L.scanl plusPtr ptr (repeat particleRenderInfoSize) :: [Ptr GLfloat]
      xs = zip ptrs (Data.Foldable.toList infos)
  in traverse_ f xs
  where f :: (Ptr GLfloat, ParticleRenderInfo) -> IO ()
        f (beg, (offset,color)) =
          let offsetBegin = beg :: Ptr GLfloat
              colorBegin = beg `plusPtr` (2*sizeOf(undefined::GLfloat))
          in do poke (castPtr offsetBegin) offset
                poke (castPtr colorBegin) color


-- This sadly does not handle anything
mappingFailureCallback MappingFailed = hPutStrLn stderr "Ooops, how could the mapping possibly go wrong!!"
mappingFailureCallback _ = pure ()


renderNaive :: VertexArrayObject -> BufferObject -> Program -> [RenderInfo]  -> IO ()
renderNaive vao objectVbo program xs  =
  let l = L.genericLength xs -- To be optimized
  -- added a few weeks later: no, this will probably NEVER be optimized
  in do blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        currentProgram $= Just program
        bindBuffer ArrayBuffer $= Just objectVbo
        withMappedBuffer ArrayBuffer WriteOnly (pokeRenderInfo xs) mappingFailureCallback
        bindVertexArrayObject $= Just vao
        drawArraysInstanced Triangles 0 6 l

renderParticle :: VertexArrayObject ->BufferObject -> Program -> [ParticleRenderInfo] -> IO()
renderParticle vao objectVbo program xs =
  let l = L.genericLength xs
  in do blendFunc $= (SrcAlpha, One)
        currentProgram $= Just program
        bindBuffer ArrayBuffer $= Just objectVbo
        withMappedBuffer ArrayBuffer WriteOnly (pokeParticleRenderInfo xs) mappingFailureCallback
        bindVertexArrayObject $= Just vao
        drawArraysInstanced Triangles 0 6 l
toGLmatrix :: (Matrix m, MatrixComponent c) => M44 c -> IO (m c)
toGLmatrix mr =
  let mc = transpose mr
  in withNewMatrix ColumnMajor (flip poke mc . castPtr)
