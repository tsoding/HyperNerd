module Main where

import qualified Bot.LogTest
import qualified Bot.PollTest
import qualified Bot.LinksTest
import qualified Bot.TwitchTest as TS
import qualified CommandTest
import qualified Sqlite.EntityPersistenceTest
import System.Exit
import Test.HUnit

main :: IO Counts
main = do
  results <-
    runTestTT $
    TestList
      [ Bot.LinksTest.spec
      , CommandTest.spec
      , Sqlite.EntityPersistenceTest.spec
      , Bot.PollTest.spec
      , Bot.LogTest.spec
      , TS.twitchResponseFromJsonSpec
      ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
