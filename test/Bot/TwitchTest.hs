{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.TwitchTest (spec) where

import Bot.Twitch
import Data.Aeson
import Data.Time
import Test.HUnit
import Text.InterpolatedString.QM

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

spec :: Test
spec = twitchResponseFromJsonSpec
