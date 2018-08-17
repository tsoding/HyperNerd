{-# LANGUAGE OverloadedStrings #-}
module BotSpec.LinksSpec (textContainsLinkSpec) where

import           Bot.Links (textContainsLink)
import           Test.HUnit

textContainsLinkSpec :: Test
textContainsLinkSpec =
    TestLabel "Text Contains Link Test" $
    TestList $ map (TestCase . assertBool "Unexpected link test result")
                   [ textContainsLink "https://google.com"
                   , textContainsLink "http://google.com"
                   , textContainsLink "google.com"
                   , textContainsLink "https://google.com/foo"
                   , textContainsLink "https://google.com/foo#bar"
                   , textContainsLink "https://google.com/foo?baz=42#bar"
                   ]
