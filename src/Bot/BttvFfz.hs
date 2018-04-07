{-# LANGUAGE OverloadedStrings #-}
module Bot.BttvFfz where

import           Bot.Replies
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Text as T
import           Effect
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Safe
import           Text.Printf

requestEmoteList :: T.Text -> String -> (Object -> Parser [T.Text]) -> Effect ()
requestEmoteList sender url emoteListExtractor =
    maybe (logMsg $ T.pack $ printf "Couldn't parse URL %s" url)
          (\request ->
               do response <- eitherDecode
                                <$> getResponseBody
                                <$> httpRequest request
                  case response >>= (parseEither emoteListExtractor) of
                    Left err -> logMsg
                                  $ T.pack
                                  $ printf "Couldn't parse Emote List response: %s" err
                    Right emotes -> replyToUser sender
                                      $ T.pack
                                      $ printf "Available emotes: %s"
                                      $ T.concat $ intersperse " "
                                      $ emotes)
          (parseRequest url)

bttvApiResponseAsEmoteList :: Object -> Parser [T.Text]
bttvApiResponseAsEmoteList obj =
    obj .: "emotes" >>= sequence . map (.: "code")

ffzApiResponseAsEmoteList :: Object -> Parser [T.Text]
ffzApiResponseAsEmoteList obj =
    do setId <- obj .: "room" >>= (.: "set") :: Parser Int
       obj .: "sets"
         >>= (.: (T.pack $ show $ setId))
         >>= (.: "emoticons")
         >>= sequence . map (.: "name")

ffzCommand :: Sender -> T.Text -> Effect ()
ffzCommand sender _ = requestEmoteList (senderName sender) url ffzApiResponseAsEmoteList
    where
      url = maybe "tsoding"
                  (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)

bttvCommand :: Sender -> T.Text -> Effect ()
bttvCommand sender _ = requestEmoteList (senderName sender) url bttvApiResponseAsEmoteList
    where
      url = maybe "tsoding"
                  (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
