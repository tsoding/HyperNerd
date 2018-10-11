module Events where

import qualified Data.Text as T

data Sender = Sender { senderName :: T.Text
                     , senderChannel :: T.Text
                     , senderSubscriber :: Bool
                     , senderMod :: Bool
                     , senderBroadcaster :: Bool
                     }

senderAuthority :: Sender -> Bool
senderAuthority sender =
    senderMod sender || senderBroadcaster sender

data Event = Join
           | Msg Sender T.Text
