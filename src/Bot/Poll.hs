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
import           Command
import           Data.Function

data PollOption = PollOption { poPollId :: Int
                             , poName :: T.Text
                             }

data Poll = Poll { pollAuthor :: T.Text
                 , pollStartedAt :: UTCTime
                 }

data Vote = Vote { voteUser :: T.Text
                 , voteOptionId :: Int
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

instance IsEntity Vote where
    toProperties vote = M.fromList [ ("user", PropertyText $ voteUser vote)
                                   , ("optionId", PropertyInt $ voteOptionId vote)
                                   ]
    fromProperties properties =
        Vote <$> extractProperty "user" properties
             <*> extractProperty "optionId" properties

pollCommand :: Sender -> [T.Text] -> Effect ()
pollCommand sender options =
    do poll <- currentPoll
       case poll of
         Just _ -> replyToSender sender "Cannot create a poll while another poll is in place"
         Nothing -> do pollId <- startPoll (senderName sender) options
                       optionsList <- return $ T.concat $ intersperse " , " options
                       say [qms|TwitchVotes The poll has been started. You have 30 seconds.
                                Use !vote command to vote for one of the options:
                                {optionsList}|]
                       timeout 30000 $ announcePollResults pollId

voteCommand :: Sender -> T.Text -> Effect ()
voteCommand sender option = do
  poll <- currentPoll
  case poll of
    Just poll' -> registerPollVote poll' sender option
    Nothing    -> replyToSender sender "No polls are in place"

pollLifetime :: UTCTime -> Entity Poll -> Double
pollLifetime currentTime pollEntity =
    realToFrac $
    diffUTCTime currentTime $
    pollStartedAt $
    entityPayload pollEntity

isPollAlive :: UTCTime -> Entity Poll -> Bool
isPollAlive currentTime pollEntity =
    pollLifetime currentTime pollEntity <= maxPollLifetime
    where maxPollLifetime = 30.0 :: Double

currentPollCommand :: CommandHandler ()
currentPollCommand sender () = do
  currentTime <- now
  poll        <- currentPoll
  case poll of
    Just poll' -> replyToSender sender [qms|id: {entityId poll'},
                                            {pollLifetime currentTime poll'}
                                            secs ago|]
    Nothing    -> replyToSender sender "No polls are in place"

currentPoll :: Effect (Maybe (Entity Poll))
currentPoll = do
  currentTime <- now
  fmap (listToMaybe . filter (isPollAlive currentTime)) $
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

announcePollResults :: Int -> Effect ()
announcePollResults pollId = do
  options <- selectEntities "PollOption" $
             Filter (PropertyEquals "pollId" $
                     PropertyInt pollId) All
  votes <- mapM (\option -> selectEntities "Vote" $
                            Filter (PropertyEquals "optionId" $
                                    PropertyInt $
                                    -- TODO(#282): how to get rid of type hint in announcePollResults?
                                    entityId option) All :: Effect [Entity Vote]) options
  let results = T.concat $
                intersperse ", " $
                map (\(option, count) -> [qms|{poName $ entityPayload $ option} : {count}|]) $
                sortBy (flip compare `on` snd) $
                zip options $
                map length votes
  say [qms|TwitchVotes Poll has finished. The results are: {results}|]

registerOptionVote :: Entity PollOption -> Sender -> Effect ()
registerOptionVote option sender = do
  existingVotes <- selectEntities "Vote" $
                   Filter (PropertyEquals "optionId" $
                           PropertyInt $
                           entityId option) All
  -- TODO: registerOptionVote filters existing votes on the haskell side
  if any ((== senderName sender) . voteUser . entityPayload) existingVotes
  then logMsg [qms|[WARNING] User {senderName sender} already
                   voted for {poName $ entityPayload option}|]
  else createEntity "Vote" Vote { voteUser = senderName sender
                                , voteOptionId = entityId option
                                } >> return ()

registerPollVote :: Entity Poll -> Sender -> T.Text -> Effect ()
registerPollVote poll sender optionName = do
  options <- selectEntities "PollOption" $
             Filter (PropertyEquals "pollId" $
                     PropertyInt $
                     entityId poll) All
  case find ((== optionName) . poName . entityPayload) options of
    Just option -> registerOptionVote option sender
    Nothing -> logMsg [qms|[WARNING] {senderName sender} voted for
                           unexisting option {optionName}|]
