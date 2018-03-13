{-# LANGUAGE OverloadedStrings #-}
module Bot.Poll where

import           Bot.Replies
import qualified Data.Text as T
import           Effect

pollCommand :: T.Text -> [T.Text] -> Effect ()
pollCommand sender _ = replyToUser sender "I don't support that yet"

voteCommand :: T.Text -> T.Text -> Effect ()
voteCommand sender _ = replyToUser sender "I don't support that yet"
