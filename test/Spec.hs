module Main where

import qualified Bot.LogSpec as LS
import qualified Bot.PollTest as PT
import qualified BotSpec.LinksSpec as BLS
import qualified BotSpec.TwitchSpec as TS
import qualified CommandSpec as CS
import qualified Sqlite.EntityPersistenceSpec as SEPS
import System.Exit
import Test.HUnit

main :: IO Counts
main = do
  results <-
    runTestTT $
    TestList
      [ BLS.textContainsLinkSpec
      , CS.commandWithGermanUmlauts
      , CS.commandWithRussians
      , CS.textAsPipeSpec
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
      , LS.testSecondsAsBackwardsDiff
      , TS.twitchResponseFromJsonSpec
      ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
