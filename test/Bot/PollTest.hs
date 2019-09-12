{-# LANGUAGE OverloadedStrings #-}

module Bot.PollTest
  ( spec
  ) where

import Bot.Poll
import Test.HUnit

testShowRanks :: Test
testShowRanks =
  TestLabel "Default scenario" $
  TestCase $ assertEqual "Unexpected result" expected actual
  where
    expected = "\"aaa\": 5, \"bbb\": 4"
    actual = showRanks [(5, "aaa" :: String), (4, "bbb")]

testRank :: Test
testRank =
  TestLabel "Default Scenario" $
  TestCase $ assertEqual "Unexpected result" expected actual
  where
    expected = [(3, "dog" :: String), (2, "cat"), (1, "mouse")]
    actual = rank ["cat" :: String, "dog", "dog", "mouse", "dog", "cat"]

testRankWithEmptyList :: Test
testRankWithEmptyList =
  TestLabel "Test when empty list is passed" $
  TestCase $ assertEqual "Unexpected result" [] (actual :: [(Int, Int)])
  where
    actual = rank []

spec :: Test
spec = TestList [testShowRanks, testRank, testRankWithEmptyList]
