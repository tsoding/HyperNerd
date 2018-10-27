module Main where

import qualified BotSpec.LinksSpec as BLS
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
      -- TODO(#225): CS.textAsPipeSpec is failing
      -- , CS.textAsPipeSpec
      , CS.textAsPipeSingleCommandSpec
      , CS.testMakePollWithNegativeDuration
      , SEPS.createEntityAndGetItById
      , SEPS.createSeveralEntityTypes
      , SEPS.deleteEntitiesWithPropertyEquals
      , SEPS.doublePrepareSchemaSpec
      -- TODO this test takes literally forever to execute which contradicts
      --  the concept of unit tests. It should probably be refactored, removed
      -- or moved to a module with longer duration tests
      -- (like integration tests or something)
      , SEPS.getRandomEntityIdWithPropertyEquals
      , SEPS.nextEntityId
      , SEPS.selectEntitiesWithPropertyEquals
      ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
