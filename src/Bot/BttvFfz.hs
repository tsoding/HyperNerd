{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import           Text.InterpolatedString.QM

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

ffzCommand :: Sender -> T.Text -> Effect ()
ffzCommand sender _ = do emotes <- requestEmoteList url ffzApiResponseAsEmoteList
                         emoteList <- return $ T.concat $ intersperse " " emotes
                         replyToSender sender [qms|Available FFZ emotes: {emoteList}|]
    where
      url = maybe "tsoding"
                  (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)

bttvCommand :: Sender -> T.Text -> Effect ()
bttvCommand sender _ = do emotes <- requestEmoteList url bttvApiResponseAsEmoteList
                          emoteList <- return $ T.concat $ intersperse " " emotes
                          replyToSender sender [qms|Available BTTV emotes: {emoteList}|]
    where
      url = maybe "tsoding"
                  (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
