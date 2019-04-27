module Main where

import qualified Bot.LogTest
import qualified Bot.PollTest
import qualified Bot.LinksTest
import qualified Bot.TwitchTest
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
      , Bot.LogTest.spec
      , Bot.PollTest.spec
      , Bot.TwitchTest.spec
      , CommandTest.spec
      , Sqlite.EntityPersistenceTest.spec
      ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
