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

newtype DubtrackRoom = DubtrackRoom
  { unName :: T.Text
  }

instance IsEntity DubtrackRoom where
  nameOfEntity _ = "DubtrackRoom"
  toProperties reply = Map.fromList [("name", PropertyText $ unName reply)]
  fromProperties = fmap DubtrackRoom . extractProperty "name"

getRoom :: Effect (Maybe (Entity DubtrackRoom))
getRoom = listToMaybe <$> selectEntities P.Proxy (Take 1 All)

setDubtrackRoom :: Reaction Message T.Text
setDubtrackRoom =
  liftR
    (\msg -> do
       mayReply <- getRoom
       case mayReply of
         Just reply ->
           void $ updateEntityById $ (\a -> a {unName = msg}) <$> reply
         Nothing -> void $ createEntity P.Proxy $ DubtrackRoom msg) $
  cmapR (const "Updated room for dubtrack") $ Reaction replyMessage

currentSongCommand :: Reaction Message ()
currentSongCommand =
  liftR (const getRoom) $
  replyOnNothing
    [qms|Dubtrack room not set, a mod can run
         '!config dubtrack <room-name>' to set it|] $
  cmapR (T.unpack . urlOfRoom . entityPayload) $
  jsonHttpRequestReaction $
  cmapR (roomCurrentSong . drData) $
  replyOnNothing "Nothing is playing right now" $
  cmapR (\song -> [qms|❝{songName song}❞: {songLink song}|]) $
  Reaction replyMessage
  where
    urlOfRoom = ("https://api.dubtrack.fm/room/" <>) . unName
