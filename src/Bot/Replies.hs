module Bot.Replies where

import qualified Data.Text as T
import           Effect
import           Text.Printf
import           Events

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.pack $ printf "@%s %s" user text

replyToSender :: Sender -> T.Text -> Effect ()
replyToSender sender = replyToUser (senderName sender)
