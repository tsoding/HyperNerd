{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Poll where

import           Bot.Replies
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
import           Text.InterpolatedString.QM

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
    fromProperties properties =
        Poll <$> extractProperty "author" properties
             <*> extractProperty "startedAt" properties

instance IsEntity PollOption where
    toProperties pollOption = M.fromList [ ("pollId", PropertyInt $ poPollId pollOption)
                                         , ("name", PropertyText $ poName pollOption)
                                         ]
    fromProperties properties =
        PollOption <$> extractProperty "pollId" properties
                   <*> extractProperty "name" properties

pollCommand :: Sender -> [T.Text] -> Effect ()
pollCommand sender options =
    do poll <- currentPoll
       case poll of
         Just _ -> replyToSender sender "Cannot create a poll while another poll is in place"
         Nothing -> do pollId <- startPoll (senderName sender) options
                       optionsList <- return $ T.concat $ intersperse ", " options
                       say [qms|The poll has been started.
                                Vote for one of the options: {optionsList}|]
                       timeout 10000 $ announcePollResults pollId

voteCommand :: Sender -> T.Text -> Effect ()
voteCommand sender option =
    do poll <- currentPoll
       case poll of
         Just poll' -> registerVote poll' (senderName sender) option
         Nothing -> replyToSender sender "No polls are in place"

alivePoll :: UTCTime -> Entity Poll -> Bool
alivePoll timeRef poll =
    (realToFrac $ diffUTCTime timeRef $ pollStartedAt $ entityPayload poll) < pollLifeTime
    where pollLifeTime = 10.0 :: Double

currentPoll :: Effect (Maybe (Entity Poll))
currentPoll = do
  timeRef <- now
  fmap (listToMaybe . filter (alivePoll timeRef)) $
    selectEntities "Poll" $
    Take 1 $
    SortBy "startedAt" Desc All

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
registerVote :: Entity Poll -> T.Text -> T.Text -> Effect ()
registerVote _ _ _ = return ()
