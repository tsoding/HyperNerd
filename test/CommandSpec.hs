{-# LANGUAGE OverloadedStrings #-}
module CommandSpec where

import           Test.HUnit
import           Command

commandWithGermanUmlauts :: Test
commandWithGermanUmlauts =
    TestLabel "Parse Command with German Umlauts" $
    TestCase $ assertEqual "Unexpected parse result"
                           (Just Command { commandName = "russify"
                                         , commandArgs = "äöü"
                                         })
                           (textAsCommand "!russify äöü")

commandWithRussians :: Test
commandWithRussians =
    TestLabel "Parse Command with Russians" $
    TestCase $ assertEqual "Unexpected parse result"
                           (Just Command { commandName = "russify"
                                         , commandArgs = "водка"
                                         })
                           (textAsCommand "!russify водка")

textAsPipeSpec :: Test
textAsPipeSpec =
    TestLabel "Parse Command Pipe" $
    TestCase $ assertEqual "Unexpected parse result"
                           ([ Command "rq" ""
                            , Command "russify" ""
                            ])
                           (textAsPipe "!rq | !russify")
