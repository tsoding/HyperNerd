module Main where

import           Russify
import           System.Exit
import           System.IO.Temp
import           Test.HUnit
import qualified SqliteEntityPersistenceSpec as SEPS

main :: IO Counts
main = do results <- runTestTT $ TestList [ SEPS.doublePrepareSchemaSpec ]
          (if errors results + failures results == 0
           then exitWith ExitSuccess
           else exitWith (ExitFailure 1))
