{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Dubtrack where

import Bot.Replies
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Proxy as P
import qualified Data.Text as T
import Effect
import Entity
import Network.HTTP.Simple
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport

data SongType
  = SongTypeYoutube
  | SongTypeSoundcloud
  deriving (Show)

instance FromJSON SongType where
  parseJSON (String "youtube") = return SongTypeYoutube
  parseJSON (String "soundcloud") = return SongTypeSoundcloud
  parseJSON invalid = typeMismatch "SongType" invalid

data Song = Song
  { songId :: T.Text
  , songType :: SongType
  , songFkId :: T.Text
  , songName :: T.Text
  } deriving (Show)

instance FromJSON Song where
  parseJSON (Object v) =
    Song <$> v .: "songid" <*> v .: "type" <*> v .: "fkid" <*> v .: "name"
  parseJSON invalid = typeMismatch "Song" invalid

newtype Room = Room
  { roomCurrentSong :: Maybe Song
  } deriving (Show)

instance FromJSON Room where
  parseJSON (Object v) = Room <$> v .: "currentSong"
  parseJSON invalid = typeMismatch "Room" invalid

data DubtrackResponse a = DubtrackResponse
  { drCode :: Int
  , drMessage :: T.Text
  , drData :: a
  }

instance FromJSON a => FromJSON (DubtrackResponse a) where
  parseJSON (Object v) =
    DubtrackResponse <$> v .: "code" <*> v .: "message" <*> v .: "data"
  parseJSON invalid = typeMismatch "DubtrackResponse" invalid

songLink :: Song -> T.Text
songLink song@(songType -> SongTypeYoutube) =
  [qms|https://www.youtube.com/watch?v={songFkId song}|]
-- TODO(#220): Soundcloud links are not supported yet
songLink (songType -> SongTypeSoundcloud) =
  "Soundcloud links are not supported yet"
songLink _ = error "This should never happen Kappa"

newtype RoomName = RoomName
  { unName :: T.Text
  }

instance IsEntity RoomName where
  nameOfEntity _ = "RoomName"
  toProperties reply = Map.fromList [("name", PropertyText $ unName reply)]
  fromProperties = fmap RoomName . extractProperty "name"

getRoom :: Effect (Entity RoomName)
getRoom = do
  reply <- listToMaybe <$> selectEntities P.Proxy (Take 1 All)
  case reply of
    Just reply' -> return reply'
    Nothing -> createEntity P.Proxy $ RoomName "tsoding"

setRoomName :: Reaction Message T.Text
setRoomName =
  liftR
    (\msg -> do
       reply <- getRoom
       void $ updateEntityById $ (\a -> a {unName = msg}) <$> reply) $
  cmapR (const "Updated room for dubtrack") $ Reaction replyMessage
  


-- TODO(#221): Dubtrack room is hardcode
currentSongCommand :: Reaction Message ()
currentSongCommand =
  Reaction $ \Message {messageSender = sender} -> do
    mahroom <- getRoom
    request <-
      parseRequest $
      "https://api.dubtrack.fm/room/" <>
      T.unpack (unName $ entityPayload mahroom)
    response <- eitherDecode . getResponseBody <$> httpRequest request
    case response of
      Left message -> errorEff $ T.pack message
      Right dubtrackResponse ->
        maybe
          (replyToSender sender "Nothing is playing right now")
          (\song ->
             replyToSender sender [qms|❝{songName song}❞: {songLink song}|])
          (roomCurrentSong $ drData dubtrackResponse)

setDubtrack :: Reaction Message T.Text
setDubtrack =
  liftR
    (\msg -> do
       reply <- getRoom
       void $ updateEntityById $ fmap (\a -> a {unName = msg}) reply) $
  cmapR (const "Updated the dubtrack") $ Reaction replyMessage
