{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module BotSpec.BttvFfzSpec where

import           Bot.BttvFfz
import           Data.Aeson
import           Data.Aeson.Types
import           Test.HUnit
import           Text.InterpolatedString.QM

parseCorrectBttvEmoteList :: Test
parseCorrectBttvEmoteList =
    TestLabel "Parse Correct BTTV Emote List" $
    TestCase $ assertEqual "Couldn't parse emote list"
                            (Right ["foo", "bar", "baz"])
                            (eitherDecode [qn| {
                                                  "emotes": [
                                                      {"code": "foo"},
                                                      {"code": "bar"},
                                                      {"code": "baz"}
                                                  ]
                                                } |]
                               >>= parseEither bttvApiResponseAsEmoteList)

parseCorrectFfzEmoteList :: Test
parseCorrectFfzEmoteList =
    TestLabel "Parse Correct FFZ Emote List" $
    TestCase $ assertEqual "Couldn't parse emote list"
                           (Right ["hello", "world"])
                           (eitherDecode [qn| {
                                                 "room": {"set": 42},
                                                 "sets": {
                                                     "42": {
                                                         "emoticons": [
                                                             {"name": "hello"},
                                                             {"name": "world"}
                                                         ]
                                                     }
                                                 }
                                             } |]
                              >>= parseEither ffzApiResponseAsEmoteList)
