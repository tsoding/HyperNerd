module Main where

import qualified Bot.ExprTest

{- Test Suite Conventions
~~~~~~~~~~~~~~~~~~~~~~~~~
1. If the module we are testing is called `Foo.Bar.Baz`,
   it should have a corresspodning module called `Foo.Bar.BazTest`.
2. `Foo.Bar.BazTest` should export only `Foo.Bar.BazTest.spec` which has
   type `Test.HUnit.Test`.
3. All of the `spec`-s from all of the `*Test` modules are accumulated in
   the `main` function and fed into `runTestTT`.
-}
import qualified Bot.FridayTest
import qualified Bot.LinksTest
import qualified Bot.LogTest
import qualified Bot.PollTest
import qualified Bot.TwitchTest
import qualified CommandTest
import qualified Data.Time.ExtraTest
import qualified OrgModeTest
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
      , Bot.FridayTest.spec
      , Bot.ExprTest.spec
      , CommandTest.spec
      , Sqlite.EntityPersistenceTest.spec
      , Data.Time.ExtraTest.spec
      , OrgModeTest.spec
      ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
