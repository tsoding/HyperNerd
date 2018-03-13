{-# LANGUAGE OverloadedStrings #-}
module Bot.Poll where

import           Bot.Replies
import           Data.List
import qualified Data.Text as T
import           Effect
import           Text.Printf

pollCommand :: T.Text -> [T.Text] -> Effect ()
pollCommand sender options =
    do poll <- currentPoll
       case poll of
         Just _ -> replyToUser sender "Cannot create a poll while another poll is in place"
         Nothing -> do pollId <- startPoll sender options
                       say
                         $ T.pack
                         $ printf "The poll has been started. Vote for one of the options: %s"
                         $ T.concat
                         $ intersperse ", " options
                       timeout 10000 $ announcePollResults pollId

voteCommand :: T.Text -> T.Text -> Effect ()
voteCommand sender _ = replyToUser sender "I don't support that yet"

-- TODO: currentPoll is not implemented yet
currentPoll :: Effect (Maybe Int)
currentPoll = return Nothing

-- TODO: startsPoll is not implemented
startPoll :: T.Text -> [T.Text] -> Effect Int
startPoll _ _ = return 42

-- TODO: announcePollResults is not implemented
announcePollResults :: Int -> Effect ()
announcePollResults _ = return ()
