{-# LANGUAGE OverloadedStrings #-}

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

fridayCommand :: Reaction Message T.Text
fridayCommand =
  transR duplicate $
  liftR
    (\msg ->
       createEntity Proxy .
       FridayVideo (messageContent msg) (senderName $ messageSender msg) =<<
       now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage

-- TODO: nextVideoCommand is not implemented
nextVideoCommand :: Reaction Message ()
nextVideoCommand = cmapR (const "Not implemented yet") $ Reaction replyMessage

-- TODO: videoCommand is not implemented
videoCommand :: Reaction Message ()
videoCommand = cmapR (const "Not implemented yet") $ Reaction replyMessage

-- TODO: setVideoDateCommand is not implemented
setVideoDateCommand :: Reaction Message UTCTime
setVideoDateCommand =
  cmapR (const "Not implemented yet") $ Reaction replyMessage
