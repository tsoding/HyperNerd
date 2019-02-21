{-# LANGUAGE DeriveFunctor #-}

module Transport where

import Control.Comonad
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Text as T
import Safe

type IncomingQueue = TQueue InEvent

type OutcomingQueue = TQueue OutEvent

data Sender = Sender
  { senderName :: T.Text
  , senderDisplayName :: T.Text
  , senderId :: T.Text
  , senderChannel :: T.Text
  , senderSubscriber :: Bool
  , senderMod :: Bool
  , senderBroadcaster :: Bool
  , senderOwner :: Bool
  }

senderAuthority :: Sender -> Bool
senderAuthority sender =
  senderMod sender || senderBroadcaster sender || senderOwner sender

data InEvent
  = Joined T.Text
  | InMsg Sender
          T.Text

newtype OutEvent =
  OutMsg T.Text

data Message a = Message
  { messageSender :: Sender
  , messageContent :: a
  } deriving (Functor)

instance Comonad Message where
  extract = messageContent
  duplicate m = m <$ m

channelOfMessage :: Message a -> T.Text
channelOfMessage Message {messageSender = sender} =
  T.pack $ fromMaybe "tsoding" $ tailMay $ T.unpack $ senderChannel sender
