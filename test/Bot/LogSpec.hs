module Bot.LogSpec where

import Bot.Log (intToSeconds, secondsAsBackwardsDiff)
import Data.Time.Clock (NominalDiffTime)
import Test.HUnit

testSecondsAsBackwardsDiff :: Test
testSecondsAsBackwardsDiff =
  TestLabel "Default Scenario" $
  TestCase $ assertEqual "Unexpected value after conversion" expected actual
  where
    expected = -5 :: NominalDiffTime
    actual = secondsAsBackwardsDiff 5

testIntToSeconds :: Test
testIntToSeconds =
  TestLabel "Check positive, negative and zero values" $
  TestCase $ assertEqual "Incorrect parsing result" expected actual
  where
    expected = [3, 0, 5]
    actual = map intToSeconds [-3, 0, 5]
