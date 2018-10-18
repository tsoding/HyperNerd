{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
module Bot.BttvFfz where

import           Bot.Replies
import           Command
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Property
import           Safe
import           Text.Printf

class Emote e where
    emoteName :: e -> T.Text

newtype FfzEmote = FfzEmote { ffzName :: T.Text }

instance IsEntity FfzEmote where
    toProperties entity =
        M.fromList [ ("name", PropertyText $ ffzName entity) ]
    fromProperties properties =
        FfzEmote <$> extractProperty "name" properties

instance Emote FfzEmote where
    emoteName = ffzName

newtype BttvEmote = BttvEmote { bttvName :: T.Text }

instance IsEntity BttvEmote where
    toProperties entity =
        M.fromList [ ("name", PropertyText $ bttvName entity) ]
    fromProperties properties =
        BttvEmote <$> extractProperty "name" properties

instance Emote BttvEmote where
    emoteName = bttvName

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

-- 😂 👌 💯 🔥
(👌) :: (a -> b) -> a -> b
f 👌 x = f x

selectEmotes :: IsEntity a => T.Text -> CommandHandler [Entity a] -> CommandHandler ()
selectEmotes name commandHandler message = do
  entities <- selectEntities name All
  commandHandler 👌 fmap (const entities) message

emotesResponse :: Emote a => [Entity a] -> T.Text
emotesResponse =
    T.concat .
    intersperse " " .
    map (emoteName . entityPayload)

replyEmotes :: Emote a => CommandHandler [Entity a]
replyEmotes = replyMessage . fmap emotesResponse

ffzApiEmotes :: CommandHandler [FfzEmote] -> CommandHandler ()
ffzApiEmotes commandHandler message@Message { messageSender = sender } = do
  let url = maybe "tsoding"
                  (printf "https://api.frankerfacez.com/v1/room/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
  emotes <- requestEmoteList url ffzApiResponseAsEmoteList
  commandHandler $ fmap (const $ map FfzEmote emotes) message

cacheFfzEmotes :: CommandHandler [FfzEmote]
cacheFfzEmotes Message { messageContent = emotes } = do
  void $ deleteEntities "FfzEmote" All
  traverse_ (createEntity "FfzEmote") emotes

bttvApiEmotes :: CommandHandler [BttvEmote] -> CommandHandler ()
bttvApiEmotes commandHandler message@Message { messageSender = sender } = do
  let url = maybe "tsoding"
                  (printf "https://api.betterttv.net/2/channels/%s" . URI.encode)
                  (tailMay $ T.unpack $ senderChannel sender)
  emotes <- requestEmoteList url bttvApiResponseAsEmoteList
  commandHandler $ fmap (const $ map BttvEmote emotes) message

cacheBttvEmotes :: CommandHandler [BttvEmote]
cacheBttvEmotes Message { messageContent = emotes } = do
  void $ deleteEntities "BttvEmote" All
  traverse_ (createEntity "BttvEmote") emotes

ffzCommand :: CommandHandler ()
ffzCommand = selectEmotes "FfzEmote" $ replyEmotes @FfzEmote

bttvCommand :: CommandHandler ()
bttvCommand = selectEmotes "BttvEmote" $ replyEmotes @BttvEmote

updateFfzEmotesCommand :: CommandHandler ()
updateFfzEmotesCommand = ffzApiEmotes cacheFfzEmotes

updateBttvEmotesCommand :: CommandHandler ()
updateBttvEmotesCommand = bttvApiEmotes cacheBttvEmotes
