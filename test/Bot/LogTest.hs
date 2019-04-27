module Bot.LogTest (spec) where

import Bot.Log (secondsAsBackwardsDiff)
import Data.Time.Clock (NominalDiffTime)
import Test.HUnit

testSecondsAsBackwardsDiff :: Test
testSecondsAsBackwardsDiff =
  TestLabel "Default Scenario" $
  TestCase $ assertEqual "Unexpected value after conversion" expected actual
  where
    expected = -5 :: NominalDiffTime
    actual = secondsAsBackwardsDiff 5

spec :: Test
spec = testSecondsAsBackwardsDiff
