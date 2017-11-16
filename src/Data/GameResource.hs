{-# LANGUAGE OverloadedStrings #-}

module Data.GameResource
  where

import Data.ByteString
import Foreign.Ptr
import Prelude hiding (readFile)

import Codec.Picture
import Data.Foldable
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL.GL
import System.IO (FilePath, IO)

data Resource = Resource
  { ballTexture :: TextureObject
  , backgroundTexture :: TextureObject
  , blockTexture :: TextureObject
  , solidBlockTexture :: TextureObject
  , paddleTexture :: TextureObject
  , shaderProgram :: Program
  , particleProgram :: Program
  }

loadShaderFromFile shaderType filePath =
  createShader shaderType >>= \s ->
    (($=) (shaderSourceBS s) =<< readFile filePath) *> compileShader s *> pure s

createProgramWith shaders = do
  program <- createProgram
  traverse_ (attachShader program) shaders
  linkProgram program
  get (programInfoLog program) >>= Prelude.putStrLn
  pure program

loadTextureFromFile :: FilePath -> IO (Either ByteString TextureObject)
loadTextureFromFile filePath = do
  Right img <- fmap convertRGBA8 <$> readImage filePath
  tex <- genObjectName :: IO TextureObject
  texOrig <- get $ textureBinding Texture2D
  textureBinding Texture2D $= Just tex
  V.unsafeWith (imageData (img)) $
    texImage2D
      Texture2D
      NoProxy
      0
      RGBA8
      (TextureSize2D
         (fromIntegral . imageWidth $ img)
         (fromIntegral . Codec.Picture.imageHeight $ img))
      0 .
    PixelData RGBA UnsignedByte . castPtr
  generateMipmap' Texture2D
  textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
  
  -- textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
  -- textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
  
  textureBinding Texture2D $= texOrig
  pure $ Right tex

loadResourceFromFiles ::
     FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO (Either ByteString Resource)
loadResourceFromFiles ballPath backgroundPath blockPath solidBlockPath paddlePath vertexShaderPath fragShaderPath particleVertexShaderPath particleFragShaderPath = do
  ballTex <- loadTextureFromFile ballPath
  backgroundTex <- loadTextureFromFile backgroundPath
  blockTex <- loadTextureFromFile blockPath
  paddleTex <- loadTextureFromFile paddlePath
  solidBlockTex <- loadTextureFromFile solidBlockPath
  vertexShader <- loadShaderFromFile VertexShader vertexShaderPath
  fragmentShader <- loadShaderFromFile FragmentShader fragShaderPath
  particleVShader <- loadShaderFromFile VertexShader particleVertexShaderPath
  particleFShader <- loadShaderFromFile FragmentShader particleFragShaderPath
  program <- createProgramWith [vertexShader, fragmentShader]
  program' <- createProgramWith [particleVShader,particleFShader]
  pure $ Resource <$> ballTex <*> backgroundTex <*> blockTex <*> solidBlockTex <*> paddleTex <*>pure program <*> pure program'
