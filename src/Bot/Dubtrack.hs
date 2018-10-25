{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Dubtrack where

import           Bot.Replies
import           Command
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import           Effect
import           Events
import           Network.HTTP.Simple
import           Text.InterpolatedString.QM

data SongType = SongTypeYoutube
              | SongTypeSoundcloud
                deriving Show

instance FromJSON SongType where
    parseJSON (String "youtube") = return SongTypeYoutube
    parseJSON (String "soundcloud") = return SongTypeSoundcloud
    parseJSON invalid = typeMismatch "SongType" invalid

data Song = Song { songId :: T.Text
                 , songType :: SongType
                 , songFkId :: T.Text
                 , songName :: T.Text
                 } deriving Show

instance FromJSON Song where
    parseJSON (Object v) = Song
        <$> v .: "songid"
        <*> v .: "type"
        <*> v .: "fkid"
        <*> v .: "name"
    parseJSON invalid = typeMismatch "Song" invalid

newtype Room = Room { roomCurrentSong :: Maybe Song } deriving Show

instance FromJSON Room where
    parseJSON (Object v) = Room <$> v .: "currentSong"
    parseJSON invalid = typeMismatch "Room" invalid

data DubtrackResponse a =
    DubtrackResponse { drCode :: Int
                     , drMessage :: T.Text
                     , drData :: a
                     }

instance FromJSON a => FromJSON (DubtrackResponse a) where
    parseJSON (Object v) = DubtrackResponse
        <$> v .: "code"
        <*> v .: "message"
        <*> v .: "data"
    parseJSON invalid = typeMismatch "DubtrackResponse" invalid

songLink :: Song -> T.Text
songLink song@(songType -> SongTypeYoutube) =
    [qms|https://www.youtube.com/watch?v={songFkId song}|]
-- TODO(#220): Soundcloud links are not supported yet
songLink (songType -> SongTypeSoundcloud) =
    "Soundcloud links are not supported yet"
songLink _ = error "This should never happen Kappa"

currentSongCommand :: CommandHandler ()
currentSongCommand Message { messageSender = sender } = do
  -- TODO(#221): Dubtrack room is hardcode
  request <- parseRequest "https://api.dubtrack.fm/room/tsoding"
  response <- eitherDecode . getResponseBody <$>
              httpRequest request
  case response of
    Left message -> errorEff $ T.pack message
    Right dubtrackResponse ->
        maybe (replyToSender sender "Nothing is playing right now")
              (\song -> replyToSender sender [qms|{songName song}: ❝{songLink song}❞|])
              (roomCurrentSong $ drData dubtrackResponse)
