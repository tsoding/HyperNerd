{-# LANGUAGE DeriveFunctor #-}

module Events where

import Control.Comonad
import qualified Data.Text as T
import Safe
import Data.Maybe

data Sender = Sender
  { senderName :: T.Text
  , senderChannel :: T.Text
  , senderSubscriber :: Bool
  , senderMod :: Bool
  , senderBroadcaster :: Bool
  , senderOwner :: Bool
  }

senderAuthority :: Sender -> Bool
senderAuthority sender =
  senderMod sender || senderBroadcaster sender || senderOwner sender

data Event
  = Join
  | Msg Sender
        T.Text

data Message a = Message
  { messageSender :: Sender
  , messageContent :: a
  } deriving (Functor)

instance Comonad Message where
  extract = messageContent
  duplicate m = const m <$> m

channelOfMessage :: Message a -> T.Text
channelOfMessage Message { messageSender = sender } =
  T.pack $ fromMaybe "tsoding" $ tailMay $ T.unpack $ senderChannel sender
