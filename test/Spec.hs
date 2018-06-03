module Main where

import qualified BotSpec.BttvFfzSpec as BFS
import qualified CommandSpec as CS
import qualified SqliteEntityPersistenceSpec as SEPS
import           System.Exit
import           Test.HUnit

main :: IO Counts
main = do results <- runTestTT $ TestList [ SEPS.doublePrepareSchemaSpec
                                          , SEPS.createEntityAndGetItById
                                          , SEPS.createSeveralEntityTypes
                                          , SEPS.nextEntityId
                                          , CS.commandWithGermanUmlauts
                                          , CS.commandWithRussians
                                          , BFS.parseCorrectBttvEmoteList
                                          , BFS.parseCorrectFfzEmoteList
                                          ]
          if errors results + failures results == 0
          then exitSuccess
          else exitWith (ExitFailure 1)
