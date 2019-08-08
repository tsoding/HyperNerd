{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Bot.Friday
  ( fridayCommand
  , nextVideoCommand
  , videoCommand
  , videoCountCommand
  , containsYtLink
  , videoQueueCommand
  , startRefreshFridayGistTimer
  , ytLinkId
  ) where

import Bot.GitHub
import Bot.Replies
import Control.Comonad
import Control.Monad
import Data.Bool.Extra
import Data.Either.Extra
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
import Data.List
import Data.Function

data FridayVideo = FridayVideo
  { fridayVideoName :: T.Text
  , fridayVideoAuthor :: T.Text
  , fridayVideoWatchedAt :: Maybe UTCTime
  , fridayVideoDate :: UTCTime
  } deriving (Show, Eq)

updateFridayVideoWatchedAt :: Maybe UTCTime -> FridayVideo -> FridayVideo
updateFridayVideoWatchedAt watchedAt video =
  video {fridayVideoWatchedAt = watchedAt}

instance IsEntity FridayVideo where
  nameOfEntity _ = "FridayVideo"
  toProperties fridayVideo =
    M.fromList $
    catMaybes
      [ return ("name", PropertyText $ fridayVideoName fridayVideo)
      , return ("author", PropertyText $ fridayVideoAuthor fridayVideo)
      , return ("date", PropertyUTCTime $ fridayVideoDate fridayVideo)
      , ("watchedAt", ) . PropertyUTCTime <$> fridayVideoWatchedAt fridayVideo
      ]
  fromProperties properties =
    FridayVideo <$> extractProperty "name" properties <*>
    extractProperty "author" properties <*>
    pure (extractProperty "watchedAt" properties) <*>
    extractProperty "date" properties

data FridayState = FridayState
  { fridayStateGistId :: Maybe GistId
  , fridayStateGistFresh :: Bool
  }

updateFridayStateGist :: GistId -> FridayState -> FridayState
updateFridayStateGist gist state = state {fridayStateGistId = Just gist}

updateFridayStateGistFresh :: Bool -> FridayState -> FridayState
updateFridayStateGistFresh fresh state = state {fridayStateGistFresh = fresh}

instance IsEntity FridayState where
  nameOfEntity _ = "LastVideoTime"
  toProperties state =
    M.fromList
      ([("gistFresh", PropertyInt $ boolAsInt $ fridayStateGistFresh state)] ++
       maybeToList
         ((,) "gistId" . PropertyText . gistIdAsText <$> fridayStateGistId state))
  fromProperties properties =
    FridayState <$> return (GistId <$> extractProperty "gistId" properties) <*>
    return (intAsBool $ fromMaybe 0 $ extractProperty "gistFresh" properties)

containsYtLink :: T.Text -> Bool
containsYtLink = isJust . ytLinkId

ytLinkId :: T.Text -> Maybe T.Text
ytLinkId text =
  (\case
     [_, _, ytId] -> return ytId
     _ -> Nothing) =<<
  rightToMaybe
    (regexParseArgs
       "https?:\\/\\/(www\\.)?youtu(be\\.com\\/watch\\?v=|\\.be\\/)([a-zA-Z0-9\\-\\_]+)"
       text)

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
         FridayVideo
           (messageContent msg)
           (senderName $ messageSender msg)
           Nothing =<<
         now) $
  cmapR (const "Added to the suggestions") $ Reaction replyMessage

sortVideos :: [Entity FridayVideo] -> [Entity FridayVideo]
sortVideos videos =
  concat $
  transpose $ groupBy (byAuthor (==)) $ sortBy (byAuthor compare) videos
  where
    byAuthor = (`on` (fridayVideoAuthor . entityPayload))

currentVideo :: [Entity FridayVideo] -> Maybe (Entity FridayVideo)
currentVideo = listToMaybe . sortVideos

unwatchedVideos :: Effect [Entity FridayVideo]
unwatchedVideos = do
  selectEntities Proxy $
    SortBy "date" Asc $ Filter (PropertyMissing "watchedAt") All

watchVideo :: Entity FridayVideo -> Effect ()
watchVideo v = do
  vt <- currentFridayState
  void $ updateEntityById (updateFridayStateGistFresh False <$> vt)
  watchedAt <- now
  void $ updateEntityById $ updateFridayVideoWatchedAt (return watchedAt) <$> v

nextVideoCommand :: Reaction Message ()
nextVideoCommand = advanceVideoQueue <> videoCommand
  where
    advanceVideoQueue =
      liftR (const unwatchedVideos) $
      cmapR currentVideo $
      ignoreNothing $
      liftR watchVideo ignore

videoCommand :: Reaction Message ()
videoCommand =
  liftR (const unwatchedVideos) $
  cmapR currentVideo $
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
    Nothing -> createEntity Proxy $ FridayState Nothing False

gistUrl :: GistId -> T.Text
gistUrl (GistId gistId) =
  [qms|https://gist.github.com/{gistId}#file-{gistFileAnchor fridayGistFileName}|]

videoCountCommand :: Reaction Message ()
videoCountCommand =
  liftR (const unwatchedVideos) $
  cmapR (T.pack . show . length) $
  Reaction replyMessage

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

fridayGistFileName :: FileName
fridayGistFileName = FileName "Queue.org"

refreshGist :: GistId -> Effect ()
refreshGist gistId = do
  gistText <- renderQueue . map entityPayload . sortVideos <$> unwatchedVideos
  updateGistFile fridayGistFileName (FileContent gistText) gistId

startRefreshFridayGistTimer :: Effect ()
startRefreshFridayGistTimer =
  periodicEffect period Nothing $ do
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

-- TODO(#648): Move configuration subcommands of !videoq to !config
videoQueueCommand :: Reaction Message T.Text
videoQueueCommand =
  subcommand
    [ ("", videoQueueLinkCommand)
    , ( "gist"
      , onlyForRoles "Only for mods" authorityRoles setVideoQueueGistCommand)
    , ( "refresh"
      , onlyForRoles "Only for mods" authorityRoles $
        liftR (const currentFridayState) $
        cmapR (updateFridayStateGistFresh False <$>) $
        liftR updateEntityById $
        cmapR (const "Freshness invalidated 👌") $ Reaction replyMessage)
    ]
