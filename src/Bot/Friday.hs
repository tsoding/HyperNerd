{-# LANGUAGE OverloadedStrings #-}

module Bot.Friday where

import Bot.Links
import Bot.Replies
import Control.Comonad
import Control.Monad.Trans.Maybe
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Property
import Reaction
import Transport (Message(..), Sender(..))
import Data.Proxy

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

messageTrustedGate :: Message a -> MaybeT Effect a
messageTrustedGate msg =
  messageContent msg <$ findTrustedSender (messageSender msg)

trustedCommand :: Reaction Message T.Text -> Reaction Message T.Text
trustedCommand reaction =
  transR duplicate $
  liftR (runMaybeT . messageTrustedGate) $
  replyOnNothing "This command is only for trusted users" reaction

fridayCommand :: Reaction Message T.Text
fridayCommand =
  trustedCommand $
  transR duplicate $
  liftR
    (\msg ->
       void $
       createEntity Proxy .
       FridayVideo (messageContent msg) (senderName $ messageSender msg) =<<
       now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage
