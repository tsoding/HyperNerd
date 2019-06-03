{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Friday
  ( fridayCommand
  , nextVideoCommand
  , videoCommand
  , setVideoDateCommand
  , videoCountCommand
  , containsYtLink
  , videoQueueCommand
  ) where

import Bot.Replies
import Control.Comonad
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Extra
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Property
import Reaction
import Regexp
import Text.InterpolatedString.QM
import Transport (Message(..), Sender(..), authorityRoles)
import Data.Functor.Compose

data FridayVideo = FridayVideo
  { fridayVideoName :: T.Text
  , fridayVideoAuthor :: T.Text
  , fridayVideoDate :: UTCTime
  } deriving (Show, Eq)

instance IsEntity FridayVideo where
  nameOfEntity _ = "FridayVideo"
  toProperties fridayVideo =
    M.fromList
      [ ("name", PropertyText $ fridayVideoName fridayVideo)
      , ("author", PropertyText $ fridayVideoAuthor fridayVideo)
      , ("date", PropertyUTCTime $ fridayVideoDate fridayVideo)
      ]
  fromProperties properties =
    FridayVideo <$> extractProperty "name" properties <*>
    extractProperty "author" properties <*>
    extractProperty "date" properties

data FridayState = FridayState
  { fridayStateTime :: UTCTime
  , fridayStateGist :: Maybe T.Text
  } deriving (Show, Eq)

updateFridayStateTime :: UTCTime -> FridayState -> FridayState
updateFridayStateTime time state = state {fridayStateTime = time}

updateFridayStateGist :: T.Text -> FridayState -> FridayState
updateFridayStateGist gist state = state {fridayStateGist = Just gist}

instance IsEntity FridayState where
  nameOfEntity _ = "LastVideoTime"
  toProperties vt =
    M.fromList
      ([("time", PropertyUTCTime $ fridayStateTime vt)] ++
       maybeToList (((,) "gistId") . PropertyText <$> fridayStateGist vt))
  fromProperties properties =
    FridayState <$> extractProperty "time" properties <*>
    return (extractProperty "gistId" properties)

containsYtLink :: T.Text -> Bool
containsYtLink =
  isRight .
  regexParseArgs
    [qn|https?:\/\/(www\.)?youtu(be\.com\/watch\?v=|\.be\/)[a-zA-Z0-9\-\_]+|]

-- TODO: fridayCommand does not update the video queue gist
fridayCommand :: Reaction Message T.Text
fridayCommand =
  cmapR (\message -> toMaybe message $ containsYtLink message) $
  replyOnNothing "You must submit a youtube link" $
  transR duplicate $
  liftR
    (\msg ->
       createEntity Proxy .
       FridayVideo (messageContent msg) (senderName $ messageSender msg) =<<
       now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage

videoQueue :: Effect [Entity FridayVideo]
videoQueue = do
  vt <- fridayStateTime . entityPayload <$> currentFridayState
  selectEntities Proxy $
    SortBy "date" Asc $ Filter (PropertyGreater "date" $ PropertyUTCTime vt) All

nextVideoCommand :: Reaction Message ()
nextVideoCommand = advanceVideoQueue <> videoCommand
  where
    advanceVideoQueue =
      liftR (const videoQueue) $
      cmapR listToMaybe $
      ignoreNothing $
      cmapR (fridayVideoDate . entityPayload) setVideoDateCommand

videoCommand :: Reaction Message ()
videoCommand =
  liftR (const videoQueue) $
  cmapR listToMaybe $
  replyOnNothing "No videos in the queue" $
  cmapR entityPayload $
  cmapR
    (\fv ->
       [qms|[{fridayVideoDate fv}] <{fridayVideoAuthor fv}> {fridayVideoName fv}|]) $
  Reaction replyMessage

currentFridayState :: Effect (Entity FridayState)
currentFridayState = do
  vt <- listToMaybe <$> selectEntities Proxy All
  case vt of
    Just vt' -> return vt'
    Nothing -> createEntity Proxy $ FridayState begginingOfTime Nothing
      where begginingOfTime = UTCTime (fromGregorian 1970 1 1) 0

gistUrl :: T.Text -> T.Text
gistUrl gistId =
  [qms|https://gist.github.com/{gistId}|]

setVideoDateCommand :: Reaction Message UTCTime
setVideoDateCommand =
  liftR
    (\newDate -> do
       vt <- currentFridayState
       updateEntityById (updateFridayStateTime newDate <$> vt)) $
  cmapR (const "Updated last video time") $ Reaction replyMessage

videoCountCommand :: Reaction Message ()
videoCountCommand =
  liftR (const videoQueue) $
  cmapR (T.pack . show . length) $ Reaction replyMessage

subcommandDispatcher ::
     M.Map T.Text (Reaction Message T.Text) -> Reaction Message T.Text
subcommandDispatcher subcommandTable =
  cmapR (regexParseArgs "([a-zA-Z0-9]*) *(.*)") $
  replyLeft $
  Reaction $ \msg ->
    case messageContent msg of
      [subcommand, args] ->
        case M.lookup subcommand subcommandTable of
          Just reaction -> runReaction reaction (args <$ msg)
          Nothing ->
            replyToSender
              (messageSender msg)
              [qms|No such subcommand {subcommand}|]
      _ -> logMsg [qms|Could not pattern match {messageContent msg}|]

videoQueueLinkCommand :: Reaction Message a
videoQueueLinkCommand =
  liftR (const currentFridayState) $
  cmapR (fridayStateGist . entityPayload) $
  replyOnNothing "Gist video queue previewing is not setup" $
  cmapR gistUrl sayMessage

setVideoQueueGistCommand :: Reaction Message T.Text
setVideoQueueGistCommand =
  liftR
    (\gist ->
       updateEntityById <$>
       getCompose (updateFridayStateGist gist <$> Compose currentFridayState)) $
  cmapR (const "Updated current Gist for Video Queue") $ Reaction replyMessage

videoQueueCommand :: Reaction Message T.Text
videoQueueCommand =
  subcommandDispatcher $
  M.fromList
    [ ("", videoQueueLinkCommand)
    , ("gist", onlyForRoles authorityRoles setVideoQueueGistCommand)
    ]
