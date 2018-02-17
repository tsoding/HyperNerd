module Main where

import           System.Exit
import           Test.HUnit
import qualified SqliteEntityPersistenceSpec as SEPS

main :: IO Counts
main = do results <- runTestTT $ TestList [ SEPS.doublePrepareSchemaSpec
                                          , SEPS.createEntityAndGetItById
                                          ]
          (if errors results + failures results == 0
           then exitWith ExitSuccess
           else exitWith (ExitFailure 1))
