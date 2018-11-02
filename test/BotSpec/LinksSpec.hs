{-# LANGUAGE OverloadedStrings #-}

module BotSpec.LinksSpec
  ( textContainsLinkSpec
  ) where

import Bot.Links (textContainsLink)
import qualified Data.Text as T
import Test.HUnit

-- TODO(#212): Some textContainsLinkSpec cases fail
textContainsLinkSpec :: Test
textContainsLinkSpec =
  TestLabel "Text Contains Link Test" $
  TestList
    [ TestCase $
      assertBool "Failed https://google.com" $
      textContainsLink "https://google.com"
    , TestCase $
      assertBool "Failed http://google.com" $
      textContainsLink "http://google.com"
    , TestCase $ assertBool "Failed google.com" $ textContainsLink "google.com"
    , TestCase $
      assertBool "Failed https://google.com/foo" $
      textContainsLink "https://google.com/foo"
    , TestCase $
      assertBool "Failed https://google.com/foo#bar" $
      textContainsLink "https://google.com/foo#bar"
    , TestCase $
      assertBool "Failed https://google.com/foo?baz=42#bar" $
      textContainsLink "https://google.com/foo?baz=42#bar"
    , TestCase $ assertBool "Failed hello" $ not $ textContainsLink "hello"
    , TestCase $
      assertBool "Failed Hello google.com World" $
      textContainsLink "Hello google.com World"
    , TestCase $
      assertBool "Failed Hello https://google.com World" $
      textContainsLink "Hello https://google.com World"
             -- Domains can be 1 character FAILS
             -- , TestCase $ assertBool "Failed t.co"                              $       textContainsLink "t.co"
             -- Domains can be more than 256 chaaracters PASSES
    , TestCase $
      assertBool "Failed long url" $
      textContainsLink $ T.concat [T.replicate 500 "t", ".co"]
             -- TLD could also be 1 character in the future FAILS
             -- , TestCase $ assertBool "Failed google.c"                          $       textContainsLink "google.c"
             -- TLD could be more than 6 characters FAILS
             -- , TestCase $ assertBool "google.cocococ"                           $       textContainsLink "google.cocococ"
    ]
