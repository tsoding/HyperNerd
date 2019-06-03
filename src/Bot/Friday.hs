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
import Network.HTTP.Simple (parseRequest, setRequestBodyLBS)

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

-- TODO: LastVideoTime and VideoQueueGist should be merged into a single metadata structure
newtype LastVideoTime = LastVideoTime
  { lastVideoTime :: UTCTime
  } deriving (Show, Eq)

instance IsEntity LastVideoTime where
  nameOfEntity _ = "LastVideoTime"
  toProperties vt = M.fromList [("time", PropertyUTCTime $ lastVideoTime vt)]
  fromProperties properties =
    LastVideoTime <$> extractProperty "time" properties

newtype VideoQueueGist = VideoQueueGist
  { videoQueueGistId :: T.Text
  } deriving (Show, Eq)

instance IsEntity VideoQueueGist where
  nameOfEntity _ = "VideoQueueGist"
  toProperties vqg =
    M.fromList [("gistid", PropertyText $ videoQueueGistId vqg)]
  fromProperties properties =
    VideoQueueGist <$> extractProperty "gistid" properties

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
  vt <- lastVideoTime . entityPayload <$> currentLastVideoTime
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

currentLastVideoTime :: Effect (Entity LastVideoTime)
currentLastVideoTime = do
  vt <- listToMaybe <$> selectEntities Proxy All
  case vt of
    Just vt' -> return vt'
    Nothing -> createEntity Proxy $ LastVideoTime begginingOfTime
      where begginingOfTime = UTCTime (fromGregorian 1970 1 1) 0

currentVideoQueueGist :: Effect (Entity VideoQueueGist)
currentVideoQueueGist = do
  vqg <- listToMaybe <$> selectEntities Proxy All
  maybe (createEntity Proxy $ VideoQueueGist "") return vqg

gistUrl :: T.Text -> T.Text
gistUrl gistId =
  [qms|https://gist.github.com/{gistId}|]

setVideoDateCommand :: Reaction Message UTCTime
setVideoDateCommand =
  liftR
    (\newDate -> do
       vt <- currentLastVideoTime
       updateEntityById (LastVideoTime newDate <$ vt)) $
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
  Reaction $ \msg -> do
    case messageContent msg of
      [subcommand, args] ->
        case M.lookup subcommand subcommandTable of
          Just reaction -> runReaction reaction (const args <$> msg)
          Nothing ->
            replyToSender
              (messageSender msg)
              [qms|No such subcommand {subcommand}|]
      _ -> logMsg [qms|Could not pattern match {messageContent msg}|]

videoQueueLinkCommand :: Reaction Message a
videoQueueLinkCommand =
  liftR (const currentVideoQueueGist) $
  cmapR (maybePredicate (not . T.null) . videoQueueGistId . entityPayload) $
  replyOnNothing "Gist video queue previewing is not setup" $
  cmapR gistUrl $ sayMessage

setVideoQueueGistCommand :: Reaction Message T.Text
setVideoQueueGistCommand =
  cmapR VideoQueueGist $
  liftR
    (\vqg ->
       getCompose (vqg <$ Compose currentVideoQueueGist)) $
  liftR updateEntityById $
  cmapR (const "Updated current Gist for Video Queue") $ Reaction replyMessage

testGistUpdate :: Reaction Message a
testGistUpdate =
  liftR
    (\_ -> do
       let body =
             "{\"files\": {\"Queue.txt\": {\"content\": \"IF YOU READ THIS I VON ZULUL\"}}}"
       request <-
         setRequestBodyLBS body <$>
         parseRequest
           "PATCH https://api.github.com/gists/62b5f871b5d57f6f776ce9a87c255b35"
       _ <- githubApiRequest request
       return "OK") $
  Reaction replyMessage

videoQueueCommand :: Reaction Message T.Text
videoQueueCommand =
  subcommandDispatcher $
  M.fromList
    [ ("", videoQueueLinkCommand)
    , ("gist", onlyForRoles authorityRoles setVideoQueueGistCommand)
    , ("test", onlyForRoles authorityRoles testGistUpdate)
    ]
