{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Friday
  ( fridayCommand
  , nextVideoCommand
  , videoCommand
  , setVideoDateCommand
  ) where

import Bot.Replies
import Control.Comonad
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Property
import Reaction
import Transport (Message(..), Sender(..))
import Data.Maybe
import Text.InterpolatedString.QM

data FridayVideo = FridayVideo
  { fridayVideoName :: T.Text
  , fridayVideoAuthor :: T.Text
  , fridayVideoDate :: UTCTime
  } deriving (Show, Eq)

instance IsEntity FridayVideo where
  nameOfEntity _ = "FridayVideo"
  toProperties fridayVideo =
    M.fromList
      [ ("name", PropertyText $ fridayVideoName fridayVideo)
      , ("author", PropertyText $ fridayVideoAuthor fridayVideo)
      , ("date", PropertyUTCTime $ fridayVideoDate fridayVideo)
      ]
  fromProperties properties =
    FridayVideo <$> extractProperty "name" properties <*>
    extractProperty "author" properties <*>
    extractProperty "date" properties

newtype LastVideoTime = LastVideoTime
  { lastVideoTime :: UTCTime
  } deriving (Show, Eq)

instance IsEntity LastVideoTime where
  nameOfEntity _ = "LastVideoTime"
  toProperties vt = M.fromList [("time", PropertyUTCTime $ lastVideoTime vt)]
  fromProperties properties =
    LastVideoTime <$> extractProperty "time" properties

fridayCommand :: Reaction Message T.Text
fridayCommand =
  transR duplicate $
  liftR
    (\msg ->
       createEntity Proxy .
       FridayVideo (messageContent msg) (senderName $ messageSender msg) =<<
       now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage

currentVideo :: Effect (Maybe (Entity FridayVideo))
currentVideo = do
  vt <- lastVideoTime . entityPayload <$> currentLastVideoTime
  fmap listToMaybe $
    selectEntities Proxy $
    SortBy "date" Asc $ Filter (PropertyGreater "date" $ PropertyUTCTime vt) All

nextVideoCommand :: Reaction Message ()
nextVideoCommand = advanceVideoQueue <> videoCommand
  where
    advanceVideoQueue =
      liftR (const currentVideo) $
      replyOnNothing "No videos in the queue" $
      cmapR (fridayVideoDate . entityPayload) $ setVideoDateCommand

videoCommand :: Reaction Message ()
videoCommand =
  liftR (const currentVideo) $
  replyOnNothing "No videos in the queue" $
  cmapR entityPayload $
  cmapR
    (\fv ->
       [qms|[{fridayVideoDate fv}] <{fridayVideoAuthor fv}> {fridayVideoName fv}|]) $
  Reaction replyMessage

currentLastVideoTime :: Effect (Entity LastVideoTime)
currentLastVideoTime = do
  vt <- listToMaybe <$> selectEntities Proxy All
  case vt of
    Just vt' -> return vt'
    Nothing -> createEntity Proxy $ LastVideoTime begginingOfTime
      where begginingOfTime = UTCTime (fromGregorian 1970 1 1) 0

setVideoDateCommand :: Reaction Message UTCTime
setVideoDateCommand =
  liftR
    (\newDate -> do
       vt <- currentLastVideoTime
       updateEntityById (LastVideoTime newDate <$ vt)) $
  cmapR (const "Updated last video time") $ Reaction replyMessage
