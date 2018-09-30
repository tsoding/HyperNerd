{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Twitch where

import           Bot.Replies
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Safe
import           Text.InterpolatedString.QM
import           Text.Printf

data TwitchStream = TwitchStream { tsStartedAt :: UTCTime
                                 , tsTitle :: T.Text
                                 }

twitchStreamsParser :: Object -> Parser [TwitchStream]
twitchStreamsParser obj =
    obj .: "data" >>= mapM (\s ->
        do title     <- s .: "title"
           startedAt <- s .: "started_at"
           return TwitchStream { tsStartedAt = startedAt
                               , tsTitle = title
                               })

twitchStreamByLogin :: T.Text -> Effect (Maybe TwitchStream)
twitchStreamByLogin login =
    do request <- parseRequest $
                  printf "https://api.twitch.tv/helix/streams?user_login=%s" $
                  URI.encode $
                  T.unpack login
       response <- twitchApiRequest request
       payload  <- return $ eitherDecode $ getResponseBody response
       either (errorEff . T.pack)
              (return . listToMaybe)
              (payload >>= parseEither twitchStreamsParser)

humanReadableDiffTime :: NominalDiffTime -> T.Text
humanReadableDiffTime t =
    T.pack
      $ unwords
      $ map (\(name, amount) -> [qms|{amount} {name}|])
      $ filter ((> 0) . snd) components
    where s :: Int
          s = round t
          components :: [(T.Text, Int)]
          components = [("days" :: T.Text, s `div` secondsInDay),
                        ("hours",   (s `mod` secondsInDay) `div` secondsInHour),
                        ("minutes", ((s `mod` secondsInDay) `mod` secondsInHour) `div` secondsInMinute),
                        ("seconds", ((s `mod` secondsInDay) `mod` secondsInHour) `mod` secondsInMinute)]
          secondsInDay = 24 * secondsInHour
          secondsInHour = 60 * secondsInMinute
          secondsInMinute = 60

uptimeCommand :: Sender -> T.Text -> Effect ()
uptimeCommand sender _ =
    do channel  <- return $
                   T.pack $
                   fromMaybe "tsoding" $
                   tailMay $
                   T.unpack $
                   senderChannel sender
       response <- twitchStreamByLogin channel
       maybe (replyToSender sender "Not even streaming LUL")
             (\twitchStream ->
                do streamStartTime   <- return $ tsStartedAt twitchStream
                   currentTime       <- now
                   humanReadableDiff <- return $
                                        humanReadableDiffTime $
                                        diffUTCTime currentTime streamStartTime
                   replyToSender sender [qms|Streaming for {humanReadableDiff}|])
             response
