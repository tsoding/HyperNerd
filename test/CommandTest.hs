{-# LANGUAGE OverloadedStrings #-}

module CommandTest where

import Command
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

textAsPipeEscapeSpec :: Test
textAsPipeEscapeSpec =
  TestLabel "Parse Command Pipe with escaped bar" $
  TestCase $
  assertEqual
    "Unexpected parse result"
    [Command "addcmd" "spoiler ||%1||", Command "cycle" ""]
    (textAsPipe "!addcmd spoiler \\|\\|%1\\|\\| | !cycle")
