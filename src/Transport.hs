{-# LANGUAGE DeriveFunctor #-}

module Transport where

import Control.Comonad
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import Safe

type IncomingQueue = TQueue InEvent

type OutcomingQueue = TQueue OutEvent

data Channel
  = DiscordChannel Word64
  | TwitchChannel T.Text
  deriving (Show, Read, Eq)

data Sender = Sender
  { senderName :: T.Text
  , senderDisplayName :: T.Text
  , senderId :: T.Text
  , senderChannel :: Channel
  , senderSubscriber :: Bool
  , senderMod :: Bool
  , senderBroadcaster :: Bool
  , senderOwner :: Bool
  }

senderAuthority :: Sender -> Bool
senderAuthority sender =
  senderMod sender || senderBroadcaster sender || senderOwner sender

data InEvent
  = Joined Channel
           T.Text
  | InMsg (Message T.Text)

newtype OutEvent =
  OutMsg T.Text

data Message a = Message
  { messageSender :: Sender
  , messageMentioned :: Bool
  , messageContent :: a
  } deriving (Functor)

instance Comonad Message where
  extract = messageContent
  duplicate m = m <$ m

twitchChannelName :: Channel -> Maybe T.Text
twitchChannelName (TwitchChannel channel) =
  Just $ T.pack $ fromMaybe "tsoding" $ tailMay $ T.unpack channel
twitchChannelName _ = Nothing
