{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BotSpec.TwitchSpec where

import Test.HUnit
import Text.InterpolatedString.QM
import Data.Aeson
import Bot.Twitch
import Data.Time

twitchResponseFromJsonSpec :: Test
twitchResponseFromJsonSpec =
  TestLabel "TwitchResponse from JSON parsing" $
  TestCase $
  assertEqual
    "Failed to parse TwitchResponse form JSON"
    (eitherDecode twitchResponseJsonString)
    (Right twitchResponse)
  where
    twitchResponseJsonString =
      [qn|{
              "data": [
                  {
                      "title": "My first PHP experience",
                      "started_at": "2018-01-01T00:00:00.000-00:00"
                  }
              ]
          }|]
    twitchResponse =
      TwitchResponse
        [ TwitchStream
            (UTCTime (fromGregorian 2018 1 1) (secondsToDiffTime 0))
            "My first PHP experience"
        ]
