{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BotState
  ( joinChannel
  , advanceTimeouts
  , handleIrcMessage
  , withBotState
  , withBotState'
  , newBotState
  , BotState(..)
  ) where

import Bot
import Config
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Free
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time
import qualified Database.SQLite.Simple as SQLite
import Effect
import Irc.Commands (ircPong, ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Ping, Privmsg), cookIrcMsg)
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import Irc.UserInfo (userNick)
import IrcTransport
import Network.HTTP.Simple
import qualified Sqlite.EntityPersistence as SEP
import System.IO
import Text.InterpolatedString.QM
import Text.Printf

data BotState = BotState
  { bsConfig :: Config
  , bsSqliteConn :: SQLite.Connection
  , bsTimeouts :: [(Integer, Effect ())]
  , bsIncoming :: IncomingQueue
  , bsOutcoming :: OutcomingQueue
  }

newBotState :: Config -> SQLite.Connection -> IO BotState
newBotState conf sqliteConn = do
  incoming <- atomically newTQueue
  outcoming <- atomically newTQueue
  return
    BotState
      { bsConfig = conf
      , bsSqliteConn = sqliteConn
      , bsTimeouts = []
      , bsIncoming = incoming
      , bsOutcoming = outcoming
      }

withBotState' :: Config -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState' conf databasePath block =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    newBotState conf sqliteConn >>= block

withBotState :: FilePath -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState configPath databasePath block = do
  conf <- configFromFile configPath
  withBotState' conf databasePath block

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

applyEffect :: (BotState, Effect ()) -> IO (BotState, Effect ())
applyEffect self@(_, Pure _) = return self
applyEffect (botState, Free (Say text s)) = do
  atomically $
    writeTQueue (bsOutcoming botState) $
    ircPrivmsg (configChannel $ bsConfig botState) $ twitchCmdEscape text
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
         hPutStr
           stderr
           [qms|[ERROR] HTTP request failed:
                {e :: HttpException}|]
         return Nothing)
  case response of
    Just response' -> return (botState, s response')
    Nothing -> return (botState, Pure ())
applyEffect (botState, Free (TwitchApiRequest request s)) = do
  let clientId = fromString $ T.unpack $ configClientId $ bsConfig botState
  response <- httpLBS (addRequestHeader "Client-ID" clientId request)
  return (botState, s response)
applyEffect (botState, Free (Timeout ms e s)) =
  return ((botState {bsTimeouts = (ms, e) : bsTimeouts botState}), s)
applyEffect (botState, Free (Listen effect s)) = do
  (botState', sayLog) <- listenEffectIO applyEffect (botState, effect)
  return (botState', s sayLog)
applyEffect (botState, Free (TwitchCommand name args s)) = do
  atomically $
    writeTQueue (bsOutcoming botState) $
    ircPrivmsg
      (configChannel $ bsConfig botState)
      [qms|/{name} {T.concat $ intersperse " " args}|]
  return (botState, s)

runEffectIO :: ((a, Effect ()) -> IO (a, Effect ())) -> (a, Effect ()) -> IO a
runEffectIO _ (x, Pure _) = return x
runEffectIO f effect = f effect >>= runEffectIO f

listenEffectIO ::
     ((a, Effect ()) -> IO (a, Effect ())) -> (a, Effect ()) -> IO (a, [T.Text])
listenEffectIO _ (x, Pure _) = return (x, [])
listenEffectIO f (x, Free (Say text s)) = do
  (x', sayLog) <- listenEffectIO f (x, s)
  return (x', text : sayLog)
listenEffectIO f effect = f effect >>= listenEffectIO f

runEffectTransIO :: BotState -> Effect () -> IO BotState
runEffectTransIO botState effect =
  SQLite.withTransaction (bsSqliteConn botState) $
  runEffectIO applyEffect (botState, effect)

joinChannel :: Bot -> BotState -> IO BotState
joinChannel b botState = runEffectTransIO botState $ b Join

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
  foldlM runEffectTransIO (botState {bsTimeouts = unripe}) $ map snd ripe
  where
    (ripe, unripe) =
      span ((<= 0) . fst) $
      sortBy (compare `on` fst) $
      map (\(t, e) -> (t - dt, e)) $ bsTimeouts botState

valueOfTag :: TagEntry -> T.Text
valueOfTag (TagEntry _ value) = value

handleIrcMessage :: Bot -> RawIrcMsg -> BotState -> IO BotState
handleIrcMessage b msg botState = do
  let badges =
        concat $
        maybeToList $
        fmap (T.splitOn "," . valueOfTag) $
        find (\(TagEntry ident _) -> ident == "badges") $ _msgTags msg
  let cookedMsg = cookIrcMsg msg
  print cookedMsg
  case cookedMsg of
    (Ping xs) -> do
      atomically $ writeTQueue (bsOutcoming botState) (ircPong xs)
      return botState
    (Privmsg userInfo target msgText) ->
      runEffectTransIO botState $
      b $
      Msg
        Sender
          { senderName = name
          , senderChannel = idText target
          , senderSubscriber = any (T.isPrefixOf "subscriber") badges
          , senderMod = any (T.isPrefixOf "moderator") badges
          , senderBroadcaster = any (T.isPrefixOf "broadcaster") badges
          , senderOwner = name == configOwner (bsConfig botState)
          }
        msgText
      where name = idText $ userNick userInfo
    _ -> return botState
