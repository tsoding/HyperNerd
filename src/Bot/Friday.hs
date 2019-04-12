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
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport (Message(..), Sender(..))

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

videoQueue :: Int -> Effect ([Entity FridayVideo])
videoQueue n = do
  vt <- lastVideoTime . entityPayload <$> currentLastVideoTime
  selectEntities Proxy $
    Take n $
    SortBy "date" Asc $ Filter (PropertyGreater "date" $ PropertyUTCTime vt) All

nextVideoCommand :: Reaction Message ()
nextVideoCommand = advanceVideoQueue <> videoCommand
  where
    advanceVideoQueue =
      liftR (const $ videoQueue 1) $
      cmapR listToMaybe $
      ignoreNothing $
      cmapR (fridayVideoDate . entityPayload) setVideoDateCommand

videoCommand :: Reaction Message ()
videoCommand =
  liftR (const $ videoQueue 1) $
  cmapR listToMaybe $
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
