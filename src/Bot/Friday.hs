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
  , fridayCountCommand
  ) where

import Bot.GitHub
import Bot.Replies
import Control.Applicative
import Control.Monad
import Data.Bool.Extra
import Data.Either.Extra
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Extra
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import HyperNerd.Comonad
import Property
import Reaction
import Regexp
import Safe
import Text.InterpolatedString.QM
import Transport (Message(..), Sender(..), authorityRoles)
import OrgMode

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
  , fridayStateCurrentUser :: Maybe T.Text
  }

updateFridayStateCurrentUser :: Maybe T.Text -> FridayState -> FridayState
updateFridayStateCurrentUser user state = state {fridayStateCurrentUser = user}

updateFridayStateGist :: GistId -> FridayState -> FridayState
updateFridayStateGist gist state = state {fridayStateGistId = Just gist}

updateFridayStateGistFresh :: Bool -> FridayState -> FridayState
updateFridayStateGistFresh fresh state = state {fridayStateGistFresh = fresh}

instance IsEntity FridayState where
  nameOfEntity _ = "LastVideoTime"
  toProperties state =
    M.fromList $
    catMaybes
      [ return
          ("gistFresh", PropertyInt $ boolAsInt $ fridayStateGistFresh state)
      , (,) "gistId" . PropertyText . gistIdAsText <$> fridayStateGistId state
      , (,) "currentUser" . PropertyText <$> fridayStateCurrentUser state
      ]
  fromProperties properties =
    FridayState <$> return (GistId <$> extractProperty "gistId" properties) <*>
    pure (intAsBool $ fromMaybe 0 $ extractProperty "gistFresh" properties) <*>
    pure (extractProperty "currentUser" properties)

containsYtLink :: T.Text -> Bool
containsYtLink = isJust . ytLinkId

ytLinkRegexp :: T.Text
ytLinkRegexp =
  "https?:\\/\\/(www\\.)?youtu(be\\.com\\/watch\\?v=|\\.be\\/)([a-zA-Z0-9_-]+)"

ytLinkId :: T.Text -> Maybe T.Text
ytLinkId text =
  (\case
     [_, _, ytId] -> return ytId
     _ -> Nothing) =<<
  rightToMaybe (regexParseArgs ytLinkRegexp text)

fridayCommand :: Reaction Message T.Text
fridayCommand =
  cmapR (\message -> toMaybe message $ containsYtLink message) $
  replyOnNothing "You must submit a youtube link" $
  transR duplicate $
  liftR
    (\msg -> do
       markGistUnfresh
       void $
         createEntity Proxy .
         FridayVideo
           (messageContent msg)
           (senderName $ messageSender msg)
           Nothing =<<
         now
       return $ messageContent msg) $
  cmapR ytLinkId $
  maybeReaction
    (Reaction $
     const $
     logMsg
       [qms|Could not check friday video
            duplicates because there is no YouTube
            id in the message.|]) $
  liftR fridayVideosByYtId $
  cmapR
    (\dups ->
       [qms|Added to the suggested video.
            This video was suggested {length dups} times|]) $
  Reaction replyMessage

unwatchedVideos :: Effect [Entity FridayVideo]
unwatchedVideos =
  selectEntities Proxy $
  SortBy "date" Asc $ Filter (PropertyMissing "watchedAt") All

type VideoQueues = M.Map T.Text [Entity FridayVideo]

queueOfVideos :: [Entity FridayVideo] -> VideoQueues
queueOfVideos =
  M.fromList .
  mapMaybe (\q -> (, q) . fridayVideoAuthor . entityPayload <$> headMay q) .
  groupBy (byAuthor (==)) . sortBy (byAuthor compare)
  where
    byAuthor = (`on` (fridayVideoAuthor . entityPayload))

nextUser :: VideoQueues -> Maybe T.Text -> Maybe T.Text
nextUser queues user
  | M.null queues = Nothing
  | otherwise = do
    let n = M.size queues
    let indexOfUser = (`mod` n) . (+ 1) <$> ((`M.lookupIndex` queues) =<< user)
    idx <- indexOfUser <|> return 0
    return $ fst $ M.elemAt idx queues

currentVideoByUser :: VideoQueues -> Maybe T.Text -> Maybe (Entity FridayVideo)
currentVideoByUser queues user
  | M.null queues = Nothing
  | otherwise = do
    user' <- user <|> return (fst $ M.elemAt 0 queues)
    headMay =<< M.lookup user' queues

currentVideo :: Effect (Maybe (Entity FridayVideo))
currentVideo = do
  queues <- queueOfVideos <$> unwatchedVideos
  user <- fridayStateCurrentUser . entityPayload <$> currentFridayState
  return $ currentVideoByUser queues user

videoQueues :: Effect (M.Map T.Text [Entity FridayVideo])
videoQueues = queueOfVideos <$> unwatchedVideos

markGistUnfresh :: Effect ()
markGistUnfresh = do
  vt <- currentFridayState
  void $ updateEntityById (updateFridayStateGistFresh False <$> vt)

nextVideoCommand :: Reaction Message ()
nextVideoCommand = Reaction (const advanceQueue) <> videoCommand
  where
    advanceQueue :: Effect ()
    advanceQueue = do
      state <- currentFridayState
      queues <- queueOfVideos <$> unwatchedVideos
      let user = fridayStateCurrentUser $ entityPayload state
      maybe
        (return ())
        (\video -> do
           watchedAt <- now
           void $
             updateEntityById $
             updateFridayVideoWatchedAt (return watchedAt) <$> video
           void $
             updateEntityById $
             updateFridayStateGistFresh False .
             updateFridayStateCurrentUser (nextUser queues user) <$>
             state) $
        currentVideoByUser queues user

videoCommand :: Reaction Message ()
videoCommand =
  liftR (const currentVideo) $
  replyOnNothing "No videos in the queue" $
  cmapR entityPayload $
  liftFst (fmap length . fridayVideosByYtId . fridayVideoName) $
  cmapR
    (\(video, times) ->
       [qms|{fridayVideoDate video}
            <{fridayVideoAuthor video}>
            {fridayVideoName video} —
            This video was suggested {times} times.|]) $
  Reaction replyMessage

currentFridayState :: Effect (Entity FridayState)
currentFridayState = do
  vt <- listToMaybe <$> selectEntities Proxy All
  case vt of
    Just vt' -> return vt'
    Nothing -> createEntity Proxy $ FridayState Nothing False Nothing

gistUrl :: GistId -> T.Text
gistUrl (GistId gistId) =
  [qms|https://gist.github.com/{gistId}#file-{gistFileAnchor fridayGistFileName}|]

videoCountCommand :: Reaction Message ()
videoCountCommand =
  liftR (const unwatchedVideos) $
  cmapR (T.pack . show . length) $ Reaction replyMessage

renderQueue :: [FridayVideo] -> T.Text
renderQueue queue@(FridayVideo {fridayVideoAuthor = user}:_) =
  [qmb|** {user}

        Video Count {length queue}\n\n
        |] <>
  renderTable
    ["Date", "Submitter", "Video", "Thumbnail"]
    (map
       (\video ->
          let ytId = fromMaybe "dQw4w9WgXcQ" $ ytLinkId $ fridayVideoName video
           in [ [qms|{fridayVideoDate video}|]
              , [qms|{fridayVideoAuthor video}|]
              , [qms|{fridayVideoName video}|]
              , [qms|[[https://img.youtube.com/vi/{ytId}/default.jpg]]|]
              ])
       queue)
renderQueue [] = ""

renderQueues :: Maybe T.Text -> VideoQueues -> T.Text
renderQueues currentUser queues =
  T.unlines $
  ([qmb|* Friday Queue

        Current User: {currentUser}

        Use ~!friday~ command to put a video here (only for trusted and subs).
        *Any video can be skipped if the streamer finds it boring.*
        |] :) $
  map (renderQueue . map entityPayload) $ M.elems queues

fridayGistFileName :: FileName
fridayGistFileName = FileName "Queue.org"

refreshGist :: GistId -> Effect ()
refreshGist gistId = do
  user <- fridayStateCurrentUser . entityPayload <$> currentFridayState
  gistText <- renderQueues user <$> videoQueues
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

fridayVideosByYtId :: T.Text -> Effect [Entity FridayVideo]
fridayVideosByYtId ytId =
  selectEntities Proxy $
  Filter (PropertyTextLike "name" ("%" <> ytId <> "%")) All

fridayCountCommand :: Reaction Message T.Text
fridayCountCommand =
  cmapR ytLinkId $
  replyOnNothing "Please submit a YouTube link" $
  liftR fridayVideosByYtId $
  cmapR (\dups -> [qms|This video was suggested {length dups} times|]) $
  Reaction replyMessage
