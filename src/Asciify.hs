{-# LANGUAGE BinaryLiterals #-}

module Asciify where

import qualified Data.Text as T
import Codec.Picture
import Data.Functor.Compose
import Data.Word
import Data.Char
import Data.Bits
import Data.List
import Debug.Trace

type Chunk = Word8


-- TODO: Individual square are upsidedown
renderChunk :: Chunk -> Char
renderChunk x = chr (bgroup * groupSize + boffset + ord '⠀')
    where bgroup = let b1 = x .&. 0b00001000 `shiftR` 3
                       b2 = (x .&. 0b10000000) `shiftR` 6
                   in fromIntegral (b1 .|. b2)
          boffset = let b1 = (x .&. 0b00000111)
                        b2 = (x .&. 0b01110000) `shiftR` 1
                    in fromIntegral (b1 .|. b2)
          groupSize = 64

greyScalePixel :: PixelRGB8 -> Pixel8
greyScalePixel (PixelRGB8 r g b) = k
    where k = round ((r' + g' + b') / 3.0)
          r' = fromIntegral r :: Float
          g' = fromIntegral g :: Float
          b' = fromIntegral b :: Float

chunkifyGreyScale :: Image Pixel8 -> [[Chunk]]
chunkifyGreyScale img =
  [ [chunkAt (i * 2, j * 4) | i <- [0 .. chunksWidth]]
  | j <- [0 .. chunksHeight]
  ]
  where
    width = imageWidth img
    height = imageHeight img
    chunksWidth = traceShowId (width `div` 2)
    chunksHeight = traceShowId (height `div` 4)
    squashBits :: [Word8] -> Word8
    squashBits = foldl' (\acc x -> shiftL acc 1 .|. x) 0
    k :: Pixel8 -> Word8
    k x
      | x < threshold = 0
      | otherwise = 1
      where
        threshold = 140
    f :: (Int, Int) -> Word8
    f (x, y)
      | 0 <= x && x < width && 0 <= y && y < height = k $ pixelAt img x y
      | otherwise = 0
    chunkAt :: (Int, Int) -> Chunk
    chunkAt (x, y) =
      squashBits $ reverse [f (i + x, j + y) | i <- [0, 1], j <- [0 .. 3]]

greyScaleImage :: DynamicImage -> Image Pixel8
greyScaleImage = pixelMap greyScalePixel . convertRGB8

asciifyGreyScale :: Image Pixel8 -> T.Text
asciifyGreyScale =
  T.unlines .
  map T.pack . getCompose . fmap renderChunk . Compose . chunkifyGreyScale

asciifyDynamicImage :: DynamicImage -> T.Text
asciifyDynamicImage = asciifyGreyScale . greyScaleImage

asciifyFile :: FilePath -> IO T.Text
asciifyFile filePath = do
  img <- readImage filePath
  case img of
    Right img' -> return $ asciifyDynamicImage img'
    Left msg -> error msg
