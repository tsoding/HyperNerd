{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module BotState
  ( advanceTimeouts
  , handleInEvent
  , withBotState
  , withBotState'
  , newBotState
  , BotState(..)
  , ChannelState(..)
  ) where

import Bot
import Config
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Function
import Data.List
import Data.String
import qualified Data.Text as T
import Data.Time
import qualified Database.SQLite.Simple as SQLite
import Effect
import Markov
import Network.HTTP.Simple
import qualified Sqlite.EntityPersistence as SEP
import System.IO
import Text.InterpolatedString.QM
import Text.Printf
import Transport

data ChannelState = ChannelState
  { csConfig :: Config
  , csIncoming :: IncomingQueue
  , csOutcoming :: OutcomingQueue
  }

data BotState = BotState
  { bsChannels :: [ChannelState]
  -- Shared
  , bsTimeouts :: [(Integer, Effect ())]
  , bsSqliteConn :: SQLite.Connection
  , bsMarkov :: Maybe Markov
  }

newChannelState :: Config -> IO ChannelState
newChannelState config = do
  incoming <- atomically newTQueue
  outcoming <- atomically newTQueue
  return
    ChannelState
      {csIncoming = incoming, csOutcoming = outcoming, csConfig = config}

newBotState :: Maybe Markov -> [Config] -> SQLite.Connection -> IO BotState
newBotState markov confs sqliteConn = do
  channels <- mapM newChannelState confs
  return
    BotState
      { bsSqliteConn = sqliteConn
      , bsTimeouts = []
      , bsMarkov = markov
      , bsChannels = channels
      }

withBotState' ::
     Maybe Markov -> [Config] -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState' markov confs databasePath block =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    newBotState markov confs sqliteConn >>= block

withBotState ::
     Maybe FilePath -> FilePath -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState markovPath tcPath databasePath block = do
  confs <- configsFromFile tcPath
  markov <- runMaybeT (MaybeT (return markovPath) >>= lift . loadMarkov)
  withBotState' markov confs databasePath block

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

channelsOfState :: ChannelState -> [Channel]
channelsOfState channelState =
  case csConfig channelState of
    TwitchConfig param -> return $ TwitchChannel $ tpChannel param
    DiscordConfig param ->
      map (DiscordChannel . fromIntegral) $ dpChannels param
    DebugConfig _ -> return $ TwitchChannel "#tsoding"

stateOfChannel :: BotState -> Channel -> Maybe ChannelState
stateOfChannel botState channel =
  find (any (== channel) . channelsOfState) $ bsChannels botState

applyEffect :: (BotState, Effect ()) -> IO (BotState, Effect ())
applyEffect self@(_, Pure _) = return self
applyEffect (botState, Free (Say channel text s)) = do
  case stateOfChannel botState channel of
    Just channelState ->
      case csConfig channelState of
        TwitchConfig _ ->
          atomically $
          writeTQueue (csOutcoming channelState) $
          OutMsg channel (twitchCmdEscape text)
        _ ->
          atomically $
          writeTQueue (csOutcoming channelState) $ OutMsg channel text
    Nothing -> hPutStrLn stderr [qms|[ERROR] Channel does not exist {channel} |]
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
applyEffect (botState, Free (UpdateEntities name selector properties s)) = do
  n <- SEP.updateEntities (bsSqliteConn botState) name selector properties
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
applyEffect (botState, Free (TwitchApiRequest channel request s)) =
  case stateOfChannel botState channel of
    Just channelState -> do
      let clientId =
            fromString $
            T.unpack $
            case csConfig channelState of
              (TwitchConfig params) -> tpTwitchClientId params
              (DiscordConfig params) -> dpTwitchClientId params
              (DebugConfig params) -> dbgTwitchClientId params
      response <- httpLBS (addRequestHeader "Client-ID" clientId request)
      return (botState, s response)
    Nothing -> do
      hPutStrLn stderr [qms|[ERROR] Channel does not exist {channel} |]
      return (botState, Pure ())
applyEffect (botState, Free (Timeout ms e s)) =
  return ((botState {bsTimeouts = (ms, e) : bsTimeouts botState}), s)
applyEffect (botState, Free (Listen effect s)) = do
  (botState', sayLog) <- listenEffectIO applyEffect (botState, effect)
  return (botState', s sayLog)
applyEffect (botState, Free (TwitchCommand channel name args s)) =
  case stateOfChannel botState channel of
    Just channelState -> do
      atomically $
        writeTQueue (csOutcoming channelState) $
        OutMsg channel [qms|/{name} {T.concat $ intersperse " " args}|]
      return (botState, s)
    Nothing -> do
      hPutStrLn stderr [qms|[ERROR] Channel does not exist {channel} |]
      return (botState, Pure ())
applyEffect (botState, Free (RandomMarkov s)) = do
  let markov = MaybeT $ return $ bsMarkov botState
  sentence <- runMaybeT (eventsAsText <$> (markov >>= lift . simulate))
  return (botState, s sentence)

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

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
  foldlM runEffectTransIO (botState {bsTimeouts = unripe}) $ map snd ripe
  where
    (ripe, unripe) =
      span ((<= 0) . fst) $
      sortBy (compare `on` fst) $
      map (\(t, e) -> (t - dt, e)) $ bsTimeouts botState

handleInEvent :: Bot -> InEvent -> BotState -> IO BotState
handleInEvent b event botState = runEffectTransIO botState $ b event
