{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Time.ExtraTest where

import Test.HUnit
import Data.Time
import Data.Time.Extra
import Text.InterpolatedString.QM
import qualified Data.Text as T

spec :: Test
spec =
  TestLabel "humanReadableDiffTime" $
  TestList $
  map
    (\(input, expected) ->
       TestCase $
       assertEqual
         [qms|Could not generate human readable message
              for nominal diff time {input}|]
         expected $
       humanReadableDiffTime input)
    testData
  where
    testData :: [(NominalDiffTime, T.Text)]
    testData = [(59, "59 seconds"), (60, "1 minutes"), (0.1, "< 1 second")]
