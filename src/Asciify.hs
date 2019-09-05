{-# LANGUAGE BinaryLiterals #-}

module Asciify (asciifyByteString, asciifyFile) where

import Codec.Picture
import Data.Bits
import Data.Char
import Data.Functor.Compose
import Data.List
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Word
import qualified Data.ByteString as BS

type Chunk = Word8

renderChunk :: Chunk -> Char
renderChunk x = chr (bgroup * groupSize + boffset + ord '⠀')
  where
    bgroup =
      let b1 = (x .&. 0b00001000) `shiftR` 3
          b2 = (x .&. 0b10000000) `shiftR` 6
       in fromIntegral (b1 .|. b2)
    boffset =
      let b1 = (x .&. 0b00000111)
          b2 = (x .&. 0b01110000) `shiftR` 1
       in fromIntegral (b1 .|. b2)
    groupSize = 64

chunkifyGreyScale :: Image Pixel8 -> [[Chunk]]
chunkifyGreyScale img =
  [ [chunkAt (i * 2, j * 4) | i <- [0 .. chunksWidth - 1]]
  | j <- [0 .. chunksHeight - 1]
  ]
  where
    width = imageWidth img
    height = imageHeight img
    chunksWidth = width `div` 2
    chunksHeight = height `div` 4
    squashBits :: [Word8] -> Word8
    squashBits = foldl' (\acc x -> shiftL acc 1 .|. x) 0
    threshold =
      let imgData = imageData img
       in round $
          (/ (fromIntegral $ V.length imgData)) $
          V.foldl' (+) (0.0 :: Float) $ V.map fromIntegral imgData
    k :: Pixel8 -> Word8
    k x
      | x < threshold = 0
      | otherwise = 1
    f :: (Int, Int) -> Word8
    f (x, y)
      | 0 <= x && x < width && 0 <= y && y < height = k $ pixelAt img x y
      | otherwise = 0
    chunkAt :: (Int, Int) -> Chunk
    chunkAt (x, y) =
      squashBits $ reverse [f (i + x, j + y) | i <- [0, 1], j <- [0 .. 3]]

greyScaleImage :: DynamicImage -> Image Pixel8
greyScaleImage = pixelMap greyScalePixel . convertRGBA8
  -- reference: https://www.mathworks.com/help/matlab/ref/rgb2gray.html
  where
    greyScalePixel :: PixelRGBA8 -> Pixel8
    greyScalePixel (PixelRGBA8 r g b a) = k
      where
        k = round ((r' * 0.299 + g' * 0.587 + b' * 0.114) * a')
        r' = fromIntegral r :: Float
        g' = fromIntegral g :: Float
        b' = fromIntegral b :: Float
        a' = (fromIntegral a :: Float) / 255.0

asciifyGreyScale :: Image Pixel8 -> T.Text
asciifyGreyScale =
  T.unwords .
  map T.pack . getCompose . fmap renderChunk . Compose . chunkifyGreyScale

resizeImageWidth :: Pixel a => Int -> Image a -> Image a
resizeImageWidth width' image
  | width /= width' =
    let ratio :: Float
        ratio = fromIntegral width' / fromIntegral width
        height' = floor (fromIntegral height * ratio)
        y_interval :: Float
        y_interval = fromIntegral height / fromIntegral height'
        x_interval :: Float
        x_interval = fromIntegral width / fromIntegral width'
        resizedData =
          [ imgData V.! idx
          | y <- [0 .. (height' - 1)]
          , x <- [0 .. (width' - 1)]
          , let idx =
                  floor (fromIntegral y * y_interval) * width +
                  floor (fromIntegral x * x_interval)
          ]
     in Image width' height' $ V.fromList resizedData
  | otherwise = image
  where
    width = imageWidth image
    height = imageHeight image
    imgData = imageData image

asciifyDynamicImage :: DynamicImage -> T.Text
asciifyDynamicImage = asciifyGreyScale . resizeImageWidth 60 . greyScaleImage

asciifyFile :: FilePath -> IO T.Text
asciifyFile filePath = do
  bytes <- BS.readFile filePath
  either error return $ asciifyByteString bytes

asciifyByteString :: BS.ByteString -> Either String T.Text
asciifyByteString bytes = asciifyDynamicImage <$> decodeImage bytes
