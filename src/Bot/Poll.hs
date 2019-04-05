{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Poll where

import Bot.Log (LogRecord(..), Seconds, getRecentLogs)
import Bot.Replies
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Property
import Reaction
import Safe
import Text.InterpolatedString.QM
import Text.Read
import Transport
import Data.Bool.Extra

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
  , pollChannel :: Maybe Channel
  }

data Vote = Vote
  { voteUser :: T.Text
  , voteOptionId :: Int
  }

instance IsEntity Poll where
  nameOfEntity _ = "Poll"
  toProperties poll =
    M.fromList
      ([ ("author", PropertyText $ pollAuthor poll)
       , ("startedAt", PropertyUTCTime $ pollStartedAt poll)
       , ("duration", PropertyInt $ pollDuration poll)
       , ("cancelled", PropertyInt $ boolAsInt $ pollCancelled poll)
       ] ++
       fmap
         ((,) "channel" . PropertyText . T.pack . show)
         (maybeToList $ pollChannel poll))
  fromProperties properties =
    Poll <$> extractProperty "author" properties <*>
    extractProperty "startedAt" properties <*>
    pure (fromMaybe 10000 $ extractProperty "duration" properties) <*>
    pure (maybe False intAsBool $ extractProperty "cancelled" properties) <*>
    pure ((readMaybe . T.unpack) =<< extractProperty "channel" properties)

instance IsEntity PollOption where
  nameOfEntity _ = "PollOption"
  toProperties pollOption =
    M.fromList
      [ ("pollId", PropertyInt $ poPollId pollOption)
      , ("name", PropertyText $ poName pollOption)
      ]
  fromProperties properties =
    PollOption <$> extractProperty "pollId" properties <*>
    extractProperty "name" properties

instance IsEntity Vote where
  nameOfEntity _ = "Vote"
  toProperties vote =
    M.fromList
      [ ("user", PropertyText $ voteUser vote)
      , ("optionId", PropertyInt $ voteOptionId vote)
      ]
  fromProperties properties =
    Vote <$> extractProperty "user" properties <*>
    extractProperty "optionId" properties

cancelPollCommand :: Reaction Message ()
cancelPollCommand =
  Reaction $ \Message {messageSender = sender} -> do
    poll <- currentPoll
    case poll of
      Just poll' -> do
        void $
          updateEntityById $
          fmap (\poll'' -> poll'' {pollCancelled = True}) poll'
        fromMaybe
          (return ())
          (say <$> pollChannel (entityPayload poll') <*>
           return [qms|TwitchVotes The current poll has been cancelled!|])
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
pollCommand :: Reaction Message (Int, [T.Text])
pollCommand =
  Reaction $ \Message { messageSender = sender
                      , messageContent = (durationSecs, options)
                      } -> do
    poll <- currentPoll
    let durationMs = durationSecs * 1000
    case poll of
      Just _ ->
        replyToSender
          sender
          "Cannot create a poll while another poll is in place"
    -- TODO(#295): passing duration of different units is not type safe
      Nothing ->
        if durationSecs >= 0
          then do
            pollId <- startPoll sender options durationMs
          -- TODO(#296): duration of poll is not human-readable in poll start announcement
            say
              (senderChannel sender)
              [qms|TwitchVotes The poll has been started.
                 You have {durationSecs} seconds:|]
            traverse_ (\(i, op) -> say (senderChannel sender) [qms|[{i}] {op}|]) $
              zip [0 :: Int ..] options
            timeout (fromIntegral durationMs) $ announcePollResults pollId
          else do
            let offset = fromInteger $ toInteger $ negate durationSecs
          -- TODO(#361): Polls with negative durations are not stored in the database
            instantlyReportResults (senderChannel sender) offset options

instantlyReportResults :: Channel -> Seconds -> [T.Text] -> Effect ()
instantlyReportResults channel durationSecs options = do
  logs <- getRecentLogs durationSecs
  unless (null logs) $ do
    let votes = filter (`elem` options) $ map lrMsg logs
    case votes of
      [] -> say channel [qms|No votes yet.|]
      _ -> do
        let ranks = rank votes
        say channel [qms|Poll results: {showRanks ranks}|]

voteMessage :: Reaction Message T.Text
voteMessage =
  cmapR (listToMaybe . T.words) $
  ignoreNothing $
  cmapR (readMaybe . T.unpack) $ ignoreNothing $ Reaction registerPollVote

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

currentPollCommand :: Reaction Message ()
currentPollCommand =
  Reaction $ \Message {messageSender = sender} -> do
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
    selectEntities Proxy $ Take 1 $ SortBy "startedAt" Desc All

startPoll :: Sender -> [T.Text] -> Int -> Effect Int
startPoll sender options duration = do
  startedAt <- now
  poll <-
    createEntity
      Proxy
      Poll
        { pollAuthor = senderName sender
        , pollStartedAt = startedAt
        , pollDuration = duration
        , pollCancelled = False
        , pollChannel = Just $ senderChannel sender
        }
  let pollId = entityId poll
  for_ options $ \name ->
    createEntity Proxy PollOption {poName = name, poPollId = pollId}
  return pollId

getOptionsAndVotesByPollId ::
     Int -> Effect ([Entity PollOption], [[Entity Vote]])
getOptionsAndVotesByPollId pollId = do
  options <-
    selectEntities Proxy $
    Filter (PropertyEquals "pollId" $ PropertyInt pollId) All
  votes <-
    mapM
      (\option ->
         selectEntities Proxy $
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
  poll <- getEntityById Proxy pollId
  unless (maybe True (pollCancelled . entityPayload) poll) $ do
    fromMaybe
      (return ())
      (say <$> (pollChannel =<< entityPayload <$> poll) <*>
       return [qms|TwitchVotes Poll has finished:|])
    traverse_
      (\(option, count) ->
         fromMaybe
           (return ())
           (say <$> (pollChannel =<< entityPayload <$> poll) <*>
            return [qms|{poName $ entityPayload $ option} : {count}|])) $
      sortBy (flip compare `on` snd) $ zip options $ map length votes

registerOptionVote :: Entity PollOption -> Sender -> Effect ()
registerOptionVote option sender = do
  existingVotes <-
    selectEntities Proxy $
    Filter (PropertyEquals "optionId" $ PropertyInt $ entityId option) All
  -- TODO(#289): registerOptionVote filters existing votes on the haskell side
  if any ((== senderName sender) . voteUser . entityPayload) existingVotes
    then logMsg
           [qms|[WARNING] User {senderName sender} already
                voted for {poName $ entityPayload option}|]
    else void $
         createEntity
           Proxy
           Vote {voteUser = senderName sender, voteOptionId = entityId option}

-- TODO(#488): poll votes are registered across the channels
registerPollVote :: Message Int -> Effect ()
registerPollVote Message {messageSender = sender, messageContent = optionNumber} = do
  poll' <- currentPoll
  case poll' of
    Just poll -> do
      options <-
        sortBy (compare `on` entityId) <$>
        selectEntities
          Proxy
          (Filter (PropertyEquals "pollId" $ PropertyInt $ entityId poll) All)
      case options `atMay` optionNumber of
        Just option -> registerOptionVote option sender
        Nothing ->
          logMsg
            [qms|[WARNING] {senderName sender} voted for
                 unexisting option {optionNumber}|]
    Nothing -> return ()

announceRunningPoll :: Channel -> Effect ()
announceRunningPoll channel = do
  poll <- currentPoll
  case poll of
    Just pollEntity
      | Just channel == pollChannel (entityPayload pollEntity) -> do
        pollOptions <-
          selectEntities Proxy $
          Filter
            (PropertyEquals "pollId" $ PropertyInt $ entityId pollEntity)
            All
        say channel "TwitchVotes The poll is still going"
        traverse_ (\(i, op) -> say channel [qms|[{i}] {op}|]) $
          zip [0 :: Int ..] $ map (poName . entityPayload) pollOptions
    _ -> return ()
