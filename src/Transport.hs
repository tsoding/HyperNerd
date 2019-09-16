{-# LANGUAGE DeriveFunctor #-}

module Transport where

import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import HyperNerd.Comonad
import Safe

type IncomingQueue = TQueue InEvent

type OutcomingQueue = TQueue OutEvent

data Transport = Transport
  { trIncoming :: IncomingQueue
  , trOutcoming :: OutcomingQueue
  }

data Channel
  = DiscordChannel Word64
  | TwitchChannel T.Text
  deriving (Show, Read, Eq)

data Role
  = TwitchSub
  | TwitchVip
  | TwitchMod
  | TwitchBroadcaster
  | TwitchBotOwner
  | DiscordRole Word64
  | DiscordGuildOwner
  | InternalRole T.Text
  deriving (Show, Eq)

tsodingTwitchedDiscordRole :: Role
tsodingTwitchedDiscordRole = DiscordRole 542590649103286273

tsodingTrustedDiscordRole :: Role
tsodingTrustedDiscordRole = DiscordRole 543864981171470346

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

authorityRoles :: [Role]
authorityRoles =
  [TwitchMod, TwitchBroadcaster, TwitchBotOwner, DiscordGuildOwner]

paidRoles :: [Role]
paidRoles = [tsodingTwitchedDiscordRole, TwitchSub]

senderAuthority :: Sender -> Bool
senderAuthority sender = any (`elem` senderRoles sender) authorityRoles

data InEvent
  = Started
  | Joined Channel
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
