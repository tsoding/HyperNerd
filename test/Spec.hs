module Main where

import           CommandSpec
import qualified SqliteEntityPersistenceSpec as SEPS
import           System.Exit
import           Test.HUnit

main :: IO Counts
main = do results <- runTestTT $ TestList [ SEPS.doublePrepareSchemaSpec
                                          , SEPS.createEntityAndGetItById
                                          , commandWithGermanUmlauts
                                          ]
          (if errors results + failures results == 0
           then exitWith ExitSuccess
           else exitWith (ExitFailure 1))
