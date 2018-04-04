module Main where

import qualified BotSpec.BttvFfzSpec as BFS
import qualified CommandSpec as CS
import qualified SqliteEntityPersistenceSpec as SEPS
import           System.Exit
import           Test.HUnit

main :: IO Counts
main = do results <- runTestTT $ TestList [ SEPS.doublePrepareSchemaSpec
                                          , SEPS.createEntityAndGetItById
                                          , CS.commandWithGermanUmlauts
                                          , CS.commandWithRussians
                                          , BFS.parseCorrectBttvEmoteList
                                          ]
          (if errors results + failures results == 0
           then exitWith ExitSuccess
           else exitWith (ExitFailure 1))
