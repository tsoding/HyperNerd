{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.BttvFfz where

import           Bot.Replies
import           Command
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Text as T
import           Effect
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Safe
import           Text.InterpolatedString.QM
import           Text.Printf

-- TODO(#274): FFZ and BTTV emotes are not cached

requestEmoteList :: String -> (Object -> Parser [T.Text]) -> Effect [T.Text]
requestEmoteList url emoteListExtractor =
    do request <- parseRequest url
       response <- eitherDecode . getResponseBody
                     <$> httpRequest request
       either (errorEff . T.pack)
              return
              (response >>= parseEither emoteListExtractor)

bttvApiResponseAsEmoteList :: Object -> Parser [T.Text]
bttvApiResponseAsEmoteList obj =
    obj .: "emotes" >>= mapM (.: "code")

ffzApiResponseAsEmoteList :: Object -> Parser [T.Text]
ffzApiResponseAsEmoteList obj =
    do setId <- obj .: "room" >>= (.: "set") :: Parser Int
       obj .: "sets"
         >>= (.: (T.pack $ show setId))
         >>= (.: "emoticons")
         >>= mapM (.: "name")

ffzCommand :: CommandHandler ()
ffzCommand Message { messageSender = sender } = do
  let url = maybe "tsoding"
                  (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
  emotes <- requestEmoteList url ffzApiResponseAsEmoteList
  let emoteList = T.concat $ intersperse " " emotes
  replyToSender sender [qms|Available FFZ emotes: {emoteList}|]

bttvCommand :: CommandHandler ()
bttvCommand Message { messageSender = sender } = do
  let url = maybe "tsoding"
                  (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
  emotes <- requestEmoteList url bttvApiResponseAsEmoteList
  let emoteList = T.concat $ intersperse " " emotes
  replyToSender sender [qms|Available BTTV emotes: {emoteList}|]
