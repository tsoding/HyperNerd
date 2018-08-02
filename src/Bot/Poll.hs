{-# LANGUAGE OverloadedStrings #-}
module Bot.Poll where

import           Bot.Replies
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf

data PollOption = PollOption { poPollId :: Int
                             , poName :: T.Text
                             }

data Poll = Poll { pollAuthor :: T.Text
                 , pollStartedAt :: UTCTime
                 }

instance IsEntity Poll where
    toProperties poll = M.fromList [ ("author", PropertyText $ pollAuthor poll)
                                   , ("startedAt", PropertyUTCTime $ pollStartedAt poll)
                                   ]
    fromEntity entity = do author    <- extractProperty "author" entity
                           startedAt <- extractProperty "startedAt" entity
                           poll      <- return Poll { pollAuthor = author
                                                    , pollStartedAt = startedAt
                                                    }
                           return (const poll <$> entity)

instance IsEntity PollOption where
    toProperties pollOption = M.fromList [ ("pollId", PropertyInt $ poPollId pollOption)
                                         , ("name", PropertyText $ poName pollOption)
                                         ]
    fromEntity entity = do pollId <- extractProperty "pollId" entity
                           name   <- extractProperty "name" entity
                           pollOption <- return PollOption { poPollId = pollId
                                                           , poName = name
                                                           }
                           return (const pollOption <$> entity)

pollCommand :: Sender -> [T.Text] -> Effect ()
pollCommand sender options =
    do poll <- currentPoll
       case poll of
         Just _ -> replyToUser (senderName sender) "Cannot create a poll while another poll is in place"
         Nothing -> do pollId <- startPoll (senderName sender) options
                       say
                         $ T.pack
                         $ printf "The poll has been started. Vote for one of the options: %s"
                         $ T.concat
                         $ intersperse ", " options
                       timeout 10000 $ announcePollResults pollId

voteCommand :: Sender -> T.Text -> Effect ()
voteCommand sender option =
    do poll <- currentPoll
       case poll of
         Just pollId -> registerVote pollId (senderName sender) option
         Nothing -> replyToUser (senderName sender) "No polls are in place"

-- TODO(#86): currentPoll is not implemented yet
currentPoll :: Effect (Maybe Int)
currentPoll = return Nothing

startPoll :: T.Text -> [T.Text] -> Effect Int
startPoll author options =
    do startedAt <- now
       poll   <- createEntity "Poll" Poll { pollAuthor = author
                                          , pollStartedAt = startedAt
                                          }
       pollId <- return $ entityId poll
       for_ options $ \name ->
           createEntity "PollOption" PollOption { poName = name
                                                , poPollId = pollId
                                                }
       return pollId

-- TODO(#88): announcePollResults is not implemented
announcePollResults :: Int -> Effect ()
announcePollResults _ = return ()

-- TODO(#89): voteCommand is not implemented
registerVote :: Int -> T.Text -> T.Text -> Effect ()
registerVote _ _ _ = return ()
