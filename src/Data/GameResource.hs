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
  , brickTexture :: TextureObject
  , shaderProgram :: Program
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
  Right img <- fmap convertRGB8 <$> readImage filePath
  tex <- genObjectName :: IO TextureObject
  texOrig <- get $ textureBinding Texture2D
  textureBinding Texture2D $= Just tex
  V.unsafeWith (imageData (img)) $
    texImage2D
      Texture2D
      NoProxy
      0
      RGB8
      (TextureSize2D
         (fromIntegral . imageWidth $ img)
         (fromIntegral . Codec.Picture.imageHeight $ img))
      0 .
    PixelData RGB UnsignedByte . castPtr
  generateMipmap' Texture2D
  textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
  textureBinding Texture2D $= texOrig
  pure $ Right tex

loadResourceFromFiles ::
     FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO (Either ByteString Resource)
loadResourceFromFiles ballPath backgroundPath brickPath vertexShaderPath fragShaderPath = do
  ballTex <- loadTextureFromFile ballPath
  backgroundTex <- loadTextureFromFile backgroundPath
  brickTex <- loadTextureFromFile brickPath
  vertexShader <- loadShaderFromFile VertexShader vertexShaderPath
  fragmentShader <- loadShaderFromFile FragmentShader fragShaderPath
  program <- createProgramWith [vertexShader, fragmentShader]
  pure $ Resource <$> ballTex <*> backgroundTex <*> brickTex <*> pure program
