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
  , startUpdateGistTimer
  ) where

import Bot.Replies
import Control.Comonad
import Control.Monad
import Data.Aeson
import Data.Bool.Extra
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Extra
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Network.HTTP.Simple (getResponseStatus, parseRequest, setRequestBodyJSON)
import Network.HTTP.Types.Status (Status(..))
import Property
import Reaction
import Regexp
import Text.InterpolatedString.QM
import Transport (Message(..), Sender(..), authorityRoles)

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
  , fridayStateGistId :: Maybe T.Text
  , fridayStateGistFresh :: Bool
  } deriving (Show, Eq)

updateFridayStateTime :: UTCTime -> FridayState -> FridayState
updateFridayStateTime time state = state {fridayStateTime = time}

updateFridayStateGist :: T.Text -> FridayState -> FridayState
updateFridayStateGist gist state = state {fridayStateGistId = Just gist}

updateFridayStateGistFresh :: Bool -> FridayState -> FridayState
updateFridayStateGistFresh fresh state = state {fridayStateGistFresh = fresh}

instance IsEntity FridayState where
  nameOfEntity _ = "LastVideoTime"
  toProperties state =
    M.fromList
      ([ ("time", PropertyUTCTime $ fridayStateTime state)
       , ("gistFresh", PropertyInt $ boolAsInt $ fridayStateGistFresh state)
       ] ++
       maybeToList ((,) "gistId" . PropertyText <$> fridayStateGistId state))
  fromProperties properties =
    FridayState <$> extractProperty "time" properties <*>
    return (extractProperty "gistId" properties) <*>
    return (intAsBool $ fromMaybe 0 $ extractProperty "gistFresh" properties)

containsYtLink :: T.Text -> Bool
containsYtLink =
  isRight .
  regexParseArgs
    [qn|https?:\/\/(www\.)?youtu(be\.com\/watch\?v=|\.be\/)[a-zA-Z0-9\-\_]+|]

fridayCommand :: Reaction Message T.Text
fridayCommand =
  cmapR (\message -> toMaybe message $ containsYtLink message) $
  replyOnNothing "You must submit a youtube link" $
  transR duplicate $
  liftR
    (\msg -> do
       state <- currentFridayState
       void $ updateEntityById $ updateFridayStateGistFresh False <$> state
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
    (\video ->
       [qms|{fridayVideoDate video}
            <{fridayVideoAuthor video}>
            {fridayVideoName video}|]) $
  Reaction replyMessage

currentFridayState :: Effect (Entity FridayState)
currentFridayState = do
  vt <- listToMaybe <$> selectEntities Proxy All
  case vt of
    Just vt' -> return vt'
    Nothing -> createEntity Proxy $ FridayState begginingOfTime Nothing False
      where begginingOfTime = UTCTime (fromGregorian 1970 1 1) 0

gistUrl :: T.Text -> T.Text
gistUrl gistId = [qms|https://gist.github.com/{gistId}|]

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

renderQueue :: [FridayVideo] -> T.Text
renderQueue queue =
  T.unlines $
  ([qms|Video Count {length queue}|] :) $
  map
    (\video ->
       [qms||{fridayVideoDate video}
            |{fridayVideoAuthor video}
            |{fridayVideoName video}||])
    queue

refreshGist :: T.Text -> Effect ()
refreshGist gistId = do
  gistText <- renderQueue . map entityPayload <$> videoQueue
  let payload =
        object
          ["files" .= object ["Queue.org" .= object ["content" .= gistText]]]
  logMsg (T.pack $ show $ encode payload)
  let request =
        setRequestBodyJSON payload <$>
        parseRequest [qms|PATCH https://api.github.com/gists/{gistId}|]
  case request of
    Right request' -> do
      response <- githubApiRequest request'
      when (statusCode (getResponseStatus response) >= 400) $
      -- TODO(#634): the GitHub API error is not logged anywhere
        logMsg [qms|[ERROR] Something went wrong with GitHub API query|]
    Left e -> logMsg [qms|[ERROR] {e}|]

startUpdateGistTimer :: Effect ()
startUpdateGistTimer =
  periodicEffect period $ do
    logMsg "[INFO] Checking if needed to update Friday Gist"
    state <- currentFridayState
    case fridayStateGistId $ entityPayload state of
      Just gistId
        | not $ fridayStateGistFresh $ entityPayload state -> do
          logMsg "[INFO] Gist is not Fresh. Updating..."
          refreshGist gistId
          void $ updateEntityById (updateFridayStateGistFresh True <$> state)
      Nothing -> logMsg "[INFO] Gist is not setup :rage:"
      _ -> logMsg "[INFO] Gist is fresh AF 👌"
  where
    period = 60 * 1000

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
  cmapR (fridayStateGistId . entityPayload) $
  replyOnNothing "Gist video queue previewing is not setup" $
  cmapR gistUrl sayMessage

setVideoQueueGistCommand :: Reaction Message T.Text
setVideoQueueGistCommand =
  liftR
    (\gist -> do
       state <- currentFridayState
       updateEntityById (updateFridayStateGist gist <$> state)) $
  cmapR (const "Updated current Gist for Video Queue") $ Reaction replyMessage

videoQueueCommand :: Reaction Message T.Text
videoQueueCommand =
  subcommandDispatcher $
  -- TODO(#635): no command to force refresh the video queue gist
  M.fromList
    [ ("", videoQueueLinkCommand)
    , ("gist", onlyForRoles authorityRoles setVideoQueueGistCommand)
    ]
