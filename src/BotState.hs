{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BotState
  ( advanceTimeouts
  , handleInEvent
  , withBotState
  , withBotState'
  , newBotState
  , destroyTimeoutsOfChannel
  , BotState(..)
  ) where

import Bot
import Config
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Database.SQLite.Simple as SQLite
import Effect
import Free
import Markov
import Network.HTTP.Simple
import qualified Sqlite.EntityPersistence as SEP
import System.IO
import System.Random
import Text.InterpolatedString.QM
import Text.Printf
import Transport

data Timeout = Timeout
  { timeoutDuration :: Integer
  , timeoutChannel :: Maybe Channel
  , timeoutEffect :: Effect ()
  }

data BotState = BotState
  { bsTwitchTransport :: Transport
  , bsDiscordTransport :: Transport
  -- Shared
  , bsTimeouts :: [Timeout]
  , bsSqliteConn :: SQLite.Connection
  , bsConfig :: Config
  , bsMarkovPath :: Maybe FilePath
  , bsMarkov :: Maybe Markov
  }

destroyTimeoutsOfChannel :: BotState -> Maybe Channel -> BotState
destroyTimeoutsOfChannel botState channel =
  botState
    {bsTimeouts = filter ((/= channel) . timeoutChannel) $ bsTimeouts botState}

newTransport :: IO Transport
newTransport = liftM2 Transport (atomically newTQueue) (atomically newTQueue)

newBotState :: Maybe FilePath -> Config -> SQLite.Connection -> IO BotState
newBotState markovPath conf sqliteConn = do
  markov <- runMaybeT (MaybeT (return markovPath) >>= lift . loadMarkov)
  twitchTransport <- newTransport
  discordTransport <- newTransport
  return
    BotState
      { bsSqliteConn = sqliteConn
      , bsTwitchTransport = twitchTransport
      , bsDiscordTransport = discordTransport
      , bsTimeouts = []
      , bsMarkovPath = markovPath
      , bsMarkov = markov
      , bsConfig = conf
      }

withBotState' ::
     Maybe FilePath -> Config -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState' markovPath conf databasePath block =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    newBotState markovPath conf sqliteConn >>= block

withBotState ::
     Maybe FilePath -> FilePath -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState markovPath tcPath databasePath block = do
  conf <- configFromFile tcPath
  withBotState' markovPath conf databasePath block

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

applyEffect :: (BotState, Effect ()) -> IO (BotState, Effect ())
applyEffect self@(_, Pure _) = return self
applyEffect (botState, Free (Say channel text s)) = do
  case channel of
    TwitchChannel _ ->
      atomically $
      writeTQueue (trOutcoming $ bsTwitchTransport botState) $
      OutMsg channel (twitchCmdEscape text)
    DiscordChannel _ ->
      atomically $
      writeTQueue (trOutcoming $ bsDiscordTransport botState) $
      OutMsg channel text
  return (botState, s)
applyEffect (botState, Free (LogMsg msg s)) = do
  putStrLn $ T.unpack msg
  return (botState, s)
applyEffect (botState, Free (Now s)) = do
  timestamp <- getCurrentTime
  return (botState, s timestamp)
applyEffect (botState, Free (ErrorEff msg)) = do
  putStrLn $ printf "[ERROR] %s" msg
  return (botState, Pure ())
applyEffect (botState, Free (CreateEntity name properties s)) = do
  entityId <- SEP.createEntity (bsSqliteConn botState) name properties
  return (botState, s entityId)
applyEffect (botState, Free (GetEntityById name entityId s)) = do
  entity <- SEP.getEntityById (bsSqliteConn botState) name entityId
  return (botState, s entity)
applyEffect (botState, Free (DeleteEntityById name entityId s)) = do
  SEP.deleteEntityById (bsSqliteConn botState) name entityId
  return (botState, s)
applyEffect (botState, Free (UpdateEntityById entity s)) = do
  entity' <- SEP.updateEntityById (bsSqliteConn botState) entity
  return (botState, s entity')
applyEffect (botState, Free (SelectEntities name selector s)) = do
  entities <- SEP.selectEntities (bsSqliteConn botState) name selector
  return (botState, s entities)
applyEffect (botState, Free (DeleteEntities name selector s)) = do
  n <- SEP.deleteEntities (bsSqliteConn botState) name selector
  return (botState, s n)
applyEffect (botState, Free (HttpRequest request s)) = do
  response <-
    catch
      (Just <$> httpLBS request)
      (\e -> do
         hPutStrLn
           stderr
           [qms|[ERROR] HTTP request failed:
                {e :: HttpException}|]
         return Nothing)
  case response of
    Just response' -> return (botState, s response')
    Nothing -> return (botState, Pure ())
applyEffect (botState, Free (TwitchApiRequest request s)) =
  case configTwitch $ bsConfig botState of
    Just TwitchConfig {tcTwitchClientId = clientId} -> do
      response <-
        httpLBS
          (addRequestHeader
             "Accept"
             (TE.encodeUtf8 "application/vnd.twitchtv.v5+json") $
           addRequestHeader "Client-ID" (TE.encodeUtf8 clientId) request)
      return (botState, s response)
    Nothing -> do
      hPutStrLn
        stderr
        [qms|[ERROR] Bot tried to perform Twitch API request.
             But Twitch clientId is not setup.|]
      return (botState, Pure ())
applyEffect (botState, Free (GitHubApiRequest request s)) = do
  let githubConfig = configGithub $ bsConfig botState
  case githubConfig of
    Just GithubConfig {githubApiKey = apiKey} -> do
      response <-
        httpLBS
          (addRequestHeader "User-Agent" "HyperNerd" $
           addRequestHeader "Authorization" [qms|token {apiKey}|] request)
      return (botState, s response)
    Nothing -> do
      hPutStrLn
        stderr
        [qms|[ERROR] Bot tried to do GitHub API request.
             But GitHub API key is not setup.|]
      return (botState, Pure ())
applyEffect (botState, Free (TimeoutEff ms e c s)) =
  return ((botState {bsTimeouts = Timeout ms e c : bsTimeouts botState}), s)
applyEffect (botState, Free (Listen effect s)) = do
  (botState', sayLog) <- listenEffectIO applyEffect (botState, effect)
  return (botState', s sayLog)
applyEffect (botState, Free (TwitchCommand channel name args s)) =
  case channel of
    TwitchChannel _ -> do
      atomically $
        writeTQueue (trOutcoming $ bsTwitchTransport botState) $
        OutMsg channel [qms|/{name} {T.concat $ intersperse " " args}|]
      return (botState, s)
    DiscordChannel _ -> do
      hPutStrLn stderr [qms|[ERROR] Discord does not support twitch commands |]
      return (botState, Pure ())
applyEffect (botState, Free (RandomMarkov s)) = do
  let markov = MaybeT $ return $ bsMarkov botState
  sentence <- runMaybeT (eventsAsText <$> (markov >>= lift . simulate))
  return (botState, s sentence)
applyEffect (botState, Free (ReloadMarkov s)) = do
  markov <-
    runMaybeT (MaybeT (return $ bsMarkovPath botState) >>= lift . loadMarkov)
  return (botState {bsMarkov = markov}, s ("Reloaded the model" <$ markov))
applyEffect (botState, Free (RandomEff range s)) = do
  v <- randomRIO range
  return (botState, s v)

runEffectIO :: ((a, Effect ()) -> IO (a, Effect ())) -> (a, Effect ()) -> IO a
runEffectIO _ (x, Pure _) = return x
runEffectIO f effect = f effect >>= runEffectIO f

listenEffectIO ::
     ((a, Effect ()) -> IO (a, Effect ())) -> (a, Effect ()) -> IO (a, [T.Text])
listenEffectIO _ (x, Pure _) = return (x, [])
listenEffectIO f (x, Free (Say _ text s)) = do
  (x', sayLog) <- listenEffectIO f (x, s)
  return (x', text : sayLog)
listenEffectIO f effect = f effect >>= listenEffectIO f

runEffectTransIO :: BotState -> Effect () -> IO BotState
runEffectTransIO botState effect =
  SQLite.withTransaction (bsSqliteConn botState) $
  runEffectIO applyEffect (botState, effect)

advanceTimeout :: Integer -> Timeout -> Timeout
advanceTimeout dt (Timeout t c e) = Timeout (t - dt) c e

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
  foldlM runEffectTransIO (botState {bsTimeouts = unripe}) $
  map timeoutEffect ripe
  where
    (ripe, unripe) =
      span ((<= 0) . timeoutDuration) $
      sortBy (compare `on` timeoutDuration) $
      map (advanceTimeout dt) $ bsTimeouts botState

handleInEvent :: Bot -> InEvent -> BotState -> IO BotState
handleInEvent b event botState = runEffectTransIO botState $ b event
