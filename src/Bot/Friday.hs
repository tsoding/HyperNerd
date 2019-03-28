{-# LANGUAGE OverloadedStrings #-}
module Bot.Friday where

import Reaction
import Transport (Message(..), Sender(..))
import qualified Data.Text as T
import Data.Time
import Entity
import qualified Data.Map as M
import Property
import Effect
import Data.Functor
import Control.Comonad
import Bot.Replies
import Control.Monad.Trans.Maybe
import Bot.Links

data FridayVideo = FridayVideo
  { fridayVideoName :: T.Text
  , fridayVideoAuthor :: T.Text
  , fridayVideoDate :: UTCTime
  } deriving (Show, Eq)

instance IsEntity FridayVideo where
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
       createEntity "FridayVideo" .
       FridayVideo (messageContent msg) (senderName $ messageSender msg) =<<
       now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage
