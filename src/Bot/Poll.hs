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
voteCommand sender option =
    do poll <- currentPoll
       case poll of
         Just pollId -> registerVote pollId sender option
         Nothing -> replyToUser sender "No polls are in place"

-- TODO(#86): currentPoll is not implemented yet
currentPoll :: Effect (Maybe Int)
currentPoll = return Nothing

-- TODO(#87): startsPoll is not implemented
startPoll :: T.Text -> [T.Text] -> Effect Int
startPoll _ _ = return 42

-- TODO: announcePollResults is not implemented
announcePollResults :: Int -> Effect ()
announcePollResults _ = return ()

-- TODO: voteCommand is not implemented
registerVote :: Int -> T.Text -> T.Text -> Effect ()
registerVote _ _ _ = return ()
