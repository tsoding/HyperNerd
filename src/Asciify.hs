{-# LANGUAGE BinaryLiterals #-}

module Asciify where

import qualified Data.Text as T
import Codec.Picture
import Data.Functor.Compose
import Data.Word
import Data.Char
import Data.Bits
import Data.List
import qualified Data.Vector.Storable as V
import Debug.Trace

type Chunk = Word8

renderChunk :: Chunk -> Char
renderChunk x = chr (bgroup * groupSize + boffset + ord '⠀')
    where bgroup = let b1 = (x .&. 0b00001000) `shiftR` 3
                       b2 = (x .&. 0b10000000) `shiftR` 6
                   in fromIntegral (b1 .|. b2)
          boffset = let b1 = (x .&. 0b00000111)
                        b2 = (x .&. 0b01110000) `shiftR` 1
                    in fromIntegral (b1 .|. b2)
          groupSize = 64

-- reference: https://www.mathworks.com/help/matlab/ref/rgb2gray.html
greyScalePixel :: PixelRGB8 -> Pixel8
greyScalePixel (PixelRGB8 r g b) = k
    where k = round (r' * 0.299  + g' * 0.587 + b' * 0.114)
          r' = fromIntegral r :: Float
          g' = fromIntegral g :: Float
          b' = fromIntegral b :: Float

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
       in traceShowId $
          round $
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

-- TODO: greyScaleImage does not handle alpha correctly (probably)
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
