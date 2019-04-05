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

data Role
  = TwitchSub
  | TwitchMod
  | TwitchBroadcaster
  | TwitchBotOwner
  | DiscordRole Word64
  | DiscordGuildOwner
  | InternalRole T.Text
  deriving (Show, Eq)

data Sender = Sender
  { senderName :: T.Text
  , senderDisplayName :: T.Text
  , senderId :: T.Text
  , senderChannel :: Channel
  , senderRoles :: [Role]
  }

senderSubscriber :: Sender -> Bool
senderSubscriber = elem TwitchSub . senderRoles

senderMod :: Sender -> Bool
senderMod = elem TwitchMod . senderRoles

senderBroadcaster :: Sender -> Bool
senderBroadcaster = elem TwitchBroadcaster . senderRoles

senderOwner :: Sender -> Bool
senderOwner = elem TwitchBotOwner . senderRoles

senderAuthority :: Sender -> Bool
senderAuthority sender =
  any
    (`elem` senderRoles sender)
    [TwitchMod, TwitchBroadcaster, TwitchBotOwner, DiscordGuildOwner]

data InEvent
  = Joined Channel
  | InMsg (Message T.Text)

data OutEvent =
  OutMsg Channel
         T.Text

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
