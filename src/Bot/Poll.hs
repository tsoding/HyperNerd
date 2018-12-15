{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Poll where

import Bot.Log (LogRecord(..), Seconds, getRecentLogs)
import Bot.Replies
import Command
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Events
import Property
import Reaction
import Text.InterpolatedString.QM

data PollOption = PollOption
  { poPollId :: Int
  , poName :: T.Text
  }

data Poll = Poll
  { pollAuthor :: T.Text
  , pollStartedAt :: UTCTime
  , pollDuration :: Int
  -- TODO(#299): Entity doesn't support boolean types
  , pollCancelled :: Bool
  }

data Vote = Vote
  { voteUser :: T.Text
  , voteOptionId :: Int
  }

voteTypeName :: T.Text
voteTypeName = "Vote"

intAsBool :: Int -> Bool
intAsBool 0 = False
intAsBool _ = True

boolAsInt :: Bool -> Int
boolAsInt True = 1
boolAsInt False = 0

instance IsEntity Poll where
  toProperties poll =
    M.fromList
      [ ("author", PropertyText $ pollAuthor poll)
      , ("startedAt", PropertyUTCTime $ pollStartedAt poll)
      , ("duration", PropertyInt $ pollDuration poll)
      , ("cancelled", PropertyInt $ boolAsInt $ pollCancelled poll)
      ]
  fromProperties properties =
    Poll <$> extractProperty "author" properties <*>
    extractProperty "startedAt" properties <*>
    pure (fromMaybe 10000 $ extractProperty "duration" properties) <*>
    pure (maybe False intAsBool $ extractProperty "cancelled" properties)

instance IsEntity PollOption where
  toProperties pollOption =
    M.fromList
      [ ("pollId", PropertyInt $ poPollId pollOption)
      , ("name", PropertyText $ poName pollOption)
      ]
  fromProperties properties =
    PollOption <$> extractProperty "pollId" properties <*>
    extractProperty "name" properties

instance IsEntity Vote where
  toProperties vote =
    M.fromList
      [ ("user", PropertyText $ voteUser vote)
      , ("optionId", PropertyInt $ voteOptionId vote)
      ]
  fromProperties properties =
    Vote <$> extractProperty "user" properties <*>
    extractProperty "optionId" properties

cancelPollCommand :: CommandHandler ()
cancelPollCommand Message {messageSender = sender} = do
  poll <- currentPoll
  case poll of
    Just poll' -> do
      void $
        updateEntityById $ fmap (\poll'' -> poll'' {pollCancelled = True}) poll'
      say [qms|TwitchVotes The current poll has been cancelled!|]
    Nothing -> replyToSender sender "No polls are in place"

-- TODO(#359): consider using rank function in implementation of announcePollResults
rank :: (Ord a) => [a] -> [(Int, a)]
rank =
  map (\l -> (length l, safeHead l)) .
  sortBy (flip compare `on` length) . group . sort
  where
    safeHead (x:_) = x
    safeHead _ = error "Empty list"

-- TODO(#360): showRanks should format results exactly like announcePollResults to make Twitch emotes visible
showRanks :: (Show a) => [(Int, a)] -> String
showRanks = intercalate ", " . map (\(i, v) -> show v ++ ": " ++ show i)

-- TODO(#294): poll duration doesn't have upper/lower limit
pollCommand :: CommandHandler (Int, [T.Text])
pollCommand Message { messageSender = sender
                    , messageContent = (durationSecs, options)
                    } = do
  poll <- currentPoll
  let durationMs = durationSecs * 1000
  case poll of
    Just _ ->
      replyToSender sender "Cannot create a poll while another poll is in place"
    -- TODO(#295): passing duration of different units is not type safe
    Nothing ->
      if durationSecs >= 0
        then do
          pollId <- startPoll sender options durationMs
          let optionsList = T.concat $ intersperse " , " options
          -- TODO(#296): duration of poll is not human-readable in poll start announcement
          say
            [qms|TwitchVotes The poll has been started. You have {durationSecs} seconds.
                 Use !vote command to vote for one of the options:
                 {optionsList}|]
          timeout (fromIntegral durationMs) $ announcePollResults pollId
        else do
          let offset = fromInteger $ toInteger $ negate durationSecs
          -- TODO(#361): Polls with negative durations are not stored in the database
          instantlyReportResults offset options

instantlyReportResults :: Seconds -> [T.Text] -> Effect ()
instantlyReportResults durationSecs options = do
  logs <- getRecentLogs durationSecs
  unless (null logs) $ do
    let votes = filter (`elem` options) $ map lrMsg logs
    case votes of
      [] -> say [qms|No votes yet.|]
      _ -> do
        let ranks = rank votes
        say [qms|Poll results: {showRanks ranks}|]

voteMessage :: Reaction Message T.Text
voteMessage =
  cmapR (,) $
  liftR (<$> currentPoll) $
  cmapR (\(option, poll) -> fmap ((,) option) poll) $
  ignoreNothing $ Reaction registerPollVote

voteCommand :: Reaction Message T.Text
voteCommand =
  cmapR (,) $
  liftR (<$> currentPoll) $
  cmapR (\(option, poll) -> fmap ((,) option) poll) $
  replyOnNothing "No polls are in place" $ Reaction registerPollVote

pollLifetime :: UTCTime -> Entity Poll -> Double
pollLifetime currentTime =
  realToFrac . diffUTCTime currentTime . pollStartedAt . entityPayload

isPollAlive :: UTCTime -> Entity Poll -> Bool
isPollAlive currentTime pollEntity =
  pollLifetime currentTime pollEntity <= maxPollLifetime &&
  not (pollCancelled poll)
  where
    maxPollLifetime = fromIntegral (pollDuration poll) * 0.001
    poll = entityPayload pollEntity

currentPollCommand :: CommandHandler ()
currentPollCommand Message {messageSender = sender} = do
  currentTime <- now
  poll <- currentPoll
  case poll of
    Just poll' ->
      replyToSender
        sender
        [qms|id: {entityId poll'},
             {pollLifetime currentTime poll'}
             secs ago|]
    Nothing -> replyToSender sender "No polls are in place"

currentPoll :: Effect (Maybe (Entity Poll))
currentPoll = do
  currentTime <- now
  fmap (listToMaybe . filter (isPollAlive currentTime)) $
    selectEntities "Poll" $ Take 1 $ SortBy "startedAt" Desc All

startPoll :: Sender -> [T.Text] -> Int -> Effect Int
startPoll sender options duration = do
  startedAt <- now
  poll <-
    createEntity
      "Poll"
      Poll
        { pollAuthor = senderName sender
        , pollStartedAt = startedAt
        , pollDuration = duration
        , pollCancelled = False
        }
  let pollId = entityId poll
  for_ options $ \name ->
    createEntity "PollOption" PollOption {poName = name, poPollId = pollId}
  return pollId

getOptionsAndVotesByPollId ::
     Int -> Effect ([Entity PollOption], [[Entity Vote]])
getOptionsAndVotesByPollId pollId = do
  options <-
    selectEntities "PollOption" $
    Filter (PropertyEquals "pollId" $ PropertyInt pollId) All
  votes <-
    mapM
      (\option ->
         selectEntities voteTypeName $
         Filter
           (PropertyEquals "optionId" $
            PropertyInt $
            -- TODO(#282): how to get rid of type hint in announcePollResults?
            entityId option)
           All :: Effect [Entity Vote])
      options
  return (options, votes)

announcePollResults :: Int -> Effect ()
announcePollResults pollId = do
  (options, votes) <- getOptionsAndVotesByPollId pollId
  let results =
        T.concat $
        intersperse ", " $
        map
          (\(option, count) ->
             [qms|{poName $ entityPayload $ option} : {count}|]) $
        sortBy (flip compare `on` snd) $ zip options $ map length votes
  poll <- getEntityById "Poll" pollId
  unless (maybe True (pollCancelled . entityPayload) poll) $
    say [qms|TwitchVotes Poll has finished. The results are: {results}|]

registerOptionVote :: Entity PollOption -> Sender -> Effect ()
registerOptionVote option sender = do
  existingVotes <-
    selectEntities "Vote" $
    Filter (PropertyEquals "optionId" $ PropertyInt $ entityId option) All
  -- TODO(#289): registerOptionVote filters existing votes on the haskell side
  if any ((== senderName sender) . voteUser . entityPayload) existingVotes
    then logMsg
           [qms|[WARNING] User {senderName sender} already
                voted for {poName $ entityPayload option}|]
    else void $
         createEntity
           "Vote"
           Vote {voteUser = senderName sender, voteOptionId = entityId option}

registerPollVote :: Message (T.Text, Entity Poll) -> Effect ()
registerPollVote Message { messageSender = sender
                         , messageContent = (optionName, poll)
                         } = do
  options <-
    selectEntities "PollOption" $
    Filter (PropertyEquals "pollId" $ PropertyInt $ entityId poll) All
  case find ((== optionName) . poName . entityPayload) options of
    Just option -> registerOptionVote option sender
    Nothing ->
      logMsg
        [qms|[WARNING] {senderName sender} voted for
             unexisting option {optionName}|]

announceRunningPoll :: Effect ()
announceRunningPoll = do
  poll <- currentPoll
  case poll of
    Just pollEntity -> do
      pollOptions <-
        selectEntities "PollOption" $
        Filter (PropertyEquals "pollId" $ PropertyInt $ entityId pollEntity) All
      let optionsList =
            T.concat $
            intersperse " , " $ map (poName . entityPayload) pollOptions
      say
        [qms|TwitchVotes The poll is still going. Use !vote command to vote for
             one of the options: {optionsList}|]
    Nothing -> return ()

unvoteCommand :: CommandHandler ()
unvoteCommand Message {messageSender = sender} = do
  maybePoll <- currentPoll
  case maybePoll of
    Just pollEntity -> do
      let pollId = entityId pollEntity
      votes <- getVotesByPollId pollId
      let name = senderName sender
      let maybeVote = findVoteByUserName name votes
      case maybeVote of
        Just vote -> do
          let voteId = entityId vote
          deleteVoteById voteId
        Nothing -> return ()
    Nothing -> replyToSender sender "No polls are in place"
  where
    deleteVoteById = deleteEntityById voteTypeName
    getVotesByPollId pollId = join . snd <$> getOptionsAndVotesByPollId pollId
    findVoteByUserName name = find (\v -> voteUser (entityPayload v) == name)
