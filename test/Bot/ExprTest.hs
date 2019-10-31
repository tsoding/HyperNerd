{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.ExprTest where

import Test.HUnit
import Bot.Expr
import HyperNerd.Parser
import qualified Data.Text as T

spec :: Test
spec =
  TestLabel "Parsing Custom Command DSL" $
  TestList $
  map
    (\(input, expected) ->
       TestCase $
       assertEqual
         ("Cannot parse `" <> input <> "` as Custom Command DSL")
         expected
         (runParser exprs $ T.pack input))
    [ ("Hello world", Right ("", [TextExpr "Hello world"]))
    , ("%Hello world", Right ("", [FunCallExpr "Hello" [], TextExpr " world"]))
    , ("%Helloworld", Right ("", [FunCallExpr "Helloworld" []]))
    , ("%Hello()world", Right ("", [FunCallExpr "Hello" [], TextExpr "world"]))
    , ( "%Hello()%world"
      , Right ("", [FunCallExpr "Hello" [], FunCallExpr "world" []]))
    , ( "%Hello ()%world"
      , Right
          ("", [FunCallExpr "Hello" [], TextExpr " ()", FunCallExpr "world" []]))
    , ( "% Hello ()%world"
      , Right ("", [TextExpr "% Hello ()", FunCallExpr "world" []]))
    , ("%%%%%%", Right ("", [TextExpr "%%%%%%"]))
    ]
