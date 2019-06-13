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
  , startRefreshFridayGistTimer
  ) where

import Bot.Replies
import Control.Comonad
import Control.Monad
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
import Property
import Reaction
import Regexp
import Text.InterpolatedString.QM
import Transport (Message(..), Sender(..), authorityRoles)
import Data.Char
import Bot.GitHub

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
  , fridayStateGistId :: Maybe GistId
  , fridayStateGistFresh :: Bool
  }

updateFridayStateTime :: UTCTime -> FridayState -> FridayState
updateFridayStateTime time state = state {fridayStateTime = time}

updateFridayStateGist :: GistId -> FridayState -> FridayState
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
       maybeToList
         ((,) "gistId" . PropertyText . gistIdAsText <$> fridayStateGistId state))
  fromProperties properties =
    FridayState <$> extractProperty "time" properties <*>
    return (GistId <$> extractProperty "gistId" properties) <*>
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

gistUrl :: GistId -> T.Text
gistUrl (GistId gistId) =
  [qms|https://gist.github.com/{gistId}#file-{anchor fridayGistFileName}|]

setVideoDateCommand :: Reaction Message UTCTime
setVideoDateCommand =
  liftR
    (\newDate -> do
       vt <- currentFridayState
       updateEntityById
         (updateFridayStateGistFresh False . updateFridayStateTime newDate <$>
          vt)) $
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

fridayGistFileName :: T.Text
fridayGistFileName = "Queue.org"

anchor :: T.Text -> T.Text
anchor =
  T.map
    (\x ->
       if isAlphaNum x
         then x
         else '-') .
  T.toLower

refreshGist :: GistId -> Effect ()
refreshGist gistId = do
  gistText <- renderQueue . map entityPayload <$> videoQueue
  updateGistFile
    (FileName fridayGistFileName)
    (FileContent gistText)
    gistId

startRefreshFridayGistTimer :: Effect ()
startRefreshFridayGistTimer =
  periodicEffect period $ do
    state <- currentFridayState
    case fridayStateGistId $ entityPayload state of
      Just gistId
        | not $ fridayStateGistFresh $ entityPayload state -> do
          logMsg "[INFO] Friday Gist is not Fresh. Updating..."
          refreshGist gistId
          void $ updateEntityById (updateFridayStateGistFresh True <$> state)
      Nothing -> logMsg "[INFO] Gist ID is not setup for Friday Page :rage:"
      _ -> logMsg "[INFO] Friday Gist is fresh AF 👌"
  where
    period = 60 * 1000

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
       updateEntityById (updateFridayStateGist (GistId gist) <$> state)) $
  cmapR (const "Updated current Gist for Video Queue") $ Reaction replyMessage

-- TODO: Move configuration subcommands of !videoq to !config
videoQueueCommand :: Reaction Message T.Text
videoQueueCommand =
  subcommand
    [ ("", videoQueueLinkCommand)
    , ("gist", onlyForRoles authorityRoles setVideoQueueGistCommand)
    , ( "refresh"
      , onlyForRoles authorityRoles $
        liftR (const currentFridayState) $
        cmapR (updateFridayStateGistFresh False <$>) $
        liftR updateEntityById $
        cmapR (const "Freshness invalidated 👌") $ Reaction replyMessage)
    ]
