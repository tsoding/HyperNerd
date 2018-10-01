module Events where

import qualified Data.Text as T

data Sender = Sender { senderName :: T.Text
                     , senderChannel :: T.Text
                     , senderSubscriber :: Bool
                     , senderMod :: Bool
                     , senderBroadcaster :: Bool
                     }

data Event = Join
           | Msg Sender T.Text
