{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Extra where

import qualified Data.Text as T
import Data.Time
import Text.InterpolatedString.QM

humanReadableDiffTime :: NominalDiffTime -> T.Text
humanReadableDiffTime t
  | t < 1.0 = "< 1 second"
  | otherwise =
    T.pack $
    unwords $
    map (\(name, amount) -> [qms|{amount} {name}|]) $
    filter ((> 0) . snd) components
  where
    s :: Int
    s = round t
    components :: [(T.Text, Int)]
    components =
      [ ("days" :: T.Text, s `div` secondsInDay)
      , ("hours", (s `mod` secondsInDay) `div` secondsInHour)
      , ( "minutes"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `div` secondsInMinute)
      , ( "seconds"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `mod` secondsInMinute)
      ]
    secondsInDay = 24 * secondsInHour
    secondsInHour = 60 * secondsInMinute
    secondsInMinute = 60
