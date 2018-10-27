{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import Bot.Poll (Poll(..), defaulPollDurationMillis, makePoll)
import Command
import Data.Text (pack)
import Data.Time (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Events (Sender(..))
import Test.HUnit

commandWithGermanUmlauts :: Test
commandWithGermanUmlauts =
  TestLabel "Parse Command with German Umlauts" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    (Just Command {commandName = "russify", commandArgs = "äöü"})
    (textAsCommand "!russify äöü")

commandWithRussians :: Test
commandWithRussians =
  TestLabel "Parse Command with Russians" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    (Just Command {commandName = "russify", commandArgs = "водка"})
    (textAsCommand "!russify водка")

textAsPipeSpec :: Test
textAsPipeSpec =
  TestLabel "Parse Command Pipe" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    [Command "rq" "", Command "russify" ""]
    (textAsPipe "!rq | !russify")

textAsPipeSingleCommandSpec :: Test
textAsPipeSingleCommandSpec =
  TestLabel "Parse Command Pipe with single command" $
  TestCase $
  assertEqual "Unexpected parse result" [Command "rq" ""] (textAsPipe "!rq")

testMakePollWithNegativeDuration :: Test
testMakePollWithNegativeDuration =
  TestLabel "Default value should be used if duration is too small" $
  TestCase $ assertEqual "Duration was not set to default" expected actual
  where
    poll = makePoll sender (-1) start
    start = UTCTime (fromGregorian 2018 10 27) 0
    expected = defaulPollDurationMillis
    actual = pollDuration poll
    sender = Sender (pack "nickname") (pack "channel") True True False
