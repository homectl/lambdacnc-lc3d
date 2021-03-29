{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaCube.GL.Data where

import           Control.Monad        (forM_, when)
import           Data.List            as L (foldl')
import           Foreign              (Ptr, Storable (peek), Word8, alloca,
                                       castPtr, nullPtr, withArray)
--import qualified Data.IntMap as IM
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as SV

--import Control.DeepSeq

import           Codec.Picture        (convertRGBA8)
import           Codec.Picture.Types  (ColorSpaceConvertible (convertImage),
                                       DynamicImage (ImageRGB8, ImageRGBA8, ImageYCbCr8),
                                       Image (Image))
import           Graphics.GL.Core33

import           LambdaCube.GL.Type

-- Buffer
disposeBuffer :: Buffer -> IO ()
disposeBuffer (Buffer _ bo) = withArray [bo] $ glDeleteBuffers 1

compileBuffer :: [Array] -> IO Buffer
compileBuffer arrs = do
    let calcDesc (offset,setters,descs) (Array arrType cnt setter) =
          let size = cnt * sizeOfArrayType arrType
          in (size + offset, (offset,size,setter):setters, ArrayDesc arrType cnt offset size:descs)
        (bufSize,arrSetters,arrDescs) = foldl' calcDesc (0,[],[]) arrs
    bo <- alloca $ \pbo -> glGenBuffers 1 pbo >> peek pbo
    glBindBuffer GL_ARRAY_BUFFER bo
    glBufferData GL_ARRAY_BUFFER (fromIntegral bufSize) nullPtr GL_STATIC_DRAW
    forM_ arrSetters $ \(offset,size,setter) -> setter $! glBufferSubData GL_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer GL_ARRAY_BUFFER 0
    return $! Buffer (V.fromList $! reverse arrDescs) bo

updateBuffer :: Buffer -> [(Int,Array)] -> IO ()
updateBuffer (Buffer arrDescs bo) arrs = do
    glBindBuffer GL_ARRAY_BUFFER bo
    forM_ arrs $ \(i,Array arrType cnt setter) -> do
        let ArrayDesc ty len offset size = arrDescs V.! i
        when (ty == arrType && cnt == len) $
            setter $! glBufferSubData GL_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
    glBindBuffer GL_ARRAY_BUFFER 0

bufferSize :: Buffer -> Int
bufferSize = V.length . bufArrays

arraySize :: Buffer -> Int -> Int
arraySize buf arrIdx = arrLength $! bufArrays buf V.! arrIdx

arrayType :: Buffer -> Int -> ArrayType
arrayType buf arrIdx = arrType $! bufArrays buf V.! arrIdx

-- Texture
disposeTexture :: TextureData -> IO ()
disposeTexture (TextureData to) = withArray [to] $ glDeleteTextures 1

-- FIXME: Temporary implemenation
uploadTexture2DToGPU :: DynamicImage -> IO TextureData
uploadTexture2DToGPU = uploadTexture2DToGPU' True False True False

uploadTexture2DToGPU' :: Bool -> Bool -> Bool -> Bool -> DynamicImage -> IO TextureData
uploadTexture2DToGPU' isFiltered isSRGB isMip isClamped bitmap' = do
    let bitmap = case bitmap' of
          ImageRGB8 Image{}     -> bitmap'
          ImageRGBA8 Image{}    -> bitmap'
          ImageYCbCr8 i@Image{} -> ImageRGB8 $ convertImage i
          di                    -> ImageRGBA8 $ convertRGBA8 di

    glPixelStorei GL_UNPACK_ALIGNMENT 1
    to <- alloca $ \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture GL_TEXTURE_2D to
    let (width,height) = bitmapSize bitmap
        texFilter = if isFiltered then GL_LINEAR else GL_NEAREST
        wrapMode = if isClamped
            then GL_CLAMP_TO_EDGE
            else GL_REPEAT
        (minFilter,maxLevel) = if not $ isFiltered && isMip
            then (texFilter,0)
            else (GL_LINEAR_MIPMAP_LINEAR, floor $ logBase 2 (fromIntegral $ max width height :: Double) :: Int)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $ fromIntegral texFilter
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral $ if isSRGB then (if nchn == 3 then GL_SRGB8 else GL_SRGB8_ALPHA8) else (if nchn == 3 then GL_RGB8 else GL_RGBA8)
            dataFormat      = fromIntegral $ case nchn of
                3 -> GL_RGB
                4 -> GL_RGBA
                _ -> error "unsupported texture format!"
        glTexImage2D GL_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat GL_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap GL_TEXTURE_2D
    return $ TextureData to


bitmapSize :: DynamicImage -> (Int, Int)
bitmapSize (ImageRGB8 (Image w h _))  = (w,h)
bitmapSize (ImageRGBA8 (Image w h _)) = (w,h)
bitmapSize _                          = error "unsupported image type :("


withBitmap :: DynamicImage -> ((Int, Int) -> Int -> Int -> Ptr Word8 -> IO a) -> IO a
withBitmap (ImageRGB8 (Image w h v)) f  = SV.unsafeWith v $ f (w,h) 3 0
withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
withBitmap _ _                          = error "unsupported image type :("
