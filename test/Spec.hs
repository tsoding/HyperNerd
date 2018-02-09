module Main where

import System.Exit
import Test.HUnit
import Russify

main :: IO Counts
main = do results <- runTestTT $ TestList []
          (if errors results + failures results == 0
           then exitWith ExitSuccess
           else exitWith (ExitFailure 1))
