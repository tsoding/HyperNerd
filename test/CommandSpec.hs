{-# LANGUAGE OverloadedStrings #-}
module CommandSpec where

import           Test.HUnit
import           Command

commandWithGermanUmlauts :: Test
commandWithGermanUmlauts =
    TestLabel "Parse Command with German Umlauts" $
    TestCase $ assertEqual "Unexpected parse result"
                           (Just $ Command { commandName = "russify"
                                           , commandArgs = "äöü"
                                           })
                           (textAsCommand "!russify äöü")
