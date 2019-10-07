{-# LANGUAGE OverloadedStrings #-}

module Bot.ExprTest where

import Bot.Expr
import Test.HUnit

-- TODO(#801): Let's fuzz Bot.Expr.exprs to make sure that it has expected behaviour
exprsTest :: Test
exprsTest =
  TestLabel "Expression parsing" $
  TestList $
  map
    (\(input, expected) ->
       TestCase $ assertEqual "" expected $ runParser exprs input)
    [ ("", Right ("", []))
    , ("hello world", Right ("", [TextExpr "hello world"]))
    , ("%x", Right ("", [VarExpr "x"]))
    , ( "%x% y  %   z"
      , Right ("", [VarExpr "x", VarExpr "y", TextExpr "  ", VarExpr "z"]))
    , ( "%x% y hello %   z"
      , Right ("", [VarExpr "x", VarExpr "y", TextExpr " hello ", VarExpr "z"]))
    , ("%f()", Right ("", [FunCallExpr "f" []]))
    , ( "%f()% g()  %  k  ()"
      , Right
          ( ""
          , [ FunCallExpr "f" []
            , FunCallExpr "g" []
            , TextExpr "  "
            , FunCallExpr "k" []
            ]))
    , ( "%f()% g() %x  %  k  ()"
      , Right
          ( ""
          , [ FunCallExpr "f" []
            , FunCallExpr "g" []
            , TextExpr " "
            , VarExpr "x"
            , TextExpr "  "
            , FunCallExpr "k" []
            ]))
    , ( "test  %f()% g() foo %x  bar %  k baz ()"
      , Right
          ( ""
          , [ TextExpr "test  "
            , FunCallExpr "f" []
            , FunCallExpr "g" []
            , TextExpr " foo "
            , VarExpr "x"
            , TextExpr "  bar "
            , VarExpr "k"
            , TextExpr " baz ()"
            ]))
    , ("%f(%x)", Right ("", [FunCallExpr "f" [VarExpr "x"]]))
    , ( "\"hello %x world\""
      , Right ("", [TextExpr "\"hello ", VarExpr "x", TextExpr " world\""]))
    ]

spec :: Test
spec = TestList [exprsTest]
