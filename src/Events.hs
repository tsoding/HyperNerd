module Events where

import qualified Data.Text as T

data Sender = Sender { senderName :: T.Text
                     , senderChannel :: T.Text
                     , senderBadges :: [T.Text]
                     }

data Event = Join
           | Msg Sender T.Text
