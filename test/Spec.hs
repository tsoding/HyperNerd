module Main where

import qualified BotSpec.LinksSpec as BLS
import qualified CommandSpec as CS
import qualified Sqlite.EntityPersistenceSpec as SEPS
import qualified Bot.PollTest as PT
import           System.Exit
import           Test.HUnit

main :: IO Counts
main = do results <- runTestTT $ TestList [ BLS.textContainsLinkSpec
                                          , CS.commandWithGermanUmlauts
                                          , CS.commandWithRussians
                                          -- TODO(#225): CS.textAsPipeSpec is failing
                                          -- , CS.textAsPipeSpec
                                          , CS.textAsPipeSingleCommandSpec
                                          , SEPS.createEntityAndGetItById
                                          , SEPS.createSeveralEntityTypes
                                          , SEPS.deleteEntitiesWithPropertyEquals
                                          , SEPS.doublePrepareSchemaSpec
                                          , SEPS.getRandomEntityIdWithPropertyEquals
                                          , SEPS.nextEntityId
                                          , SEPS.selectEntitiesWithPropertyEquals
                                          , PT.testShowRanks
                                          , PT.testRank
                                          , PT.testRankWithEmptyList
                                          ]
          if errors results + failures results == 0
          then exitSuccess
          else exitWith (ExitFailure 1)
