{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BotState
  ( advanceTimeouts
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time
import qualified Database.SQLite.Simple as SQLite
import Effect
import Events
import Irc.Commands (ircPong, ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Join, Ping, Privmsg), cookIrcMsg)
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import Irc.UserInfo (userNick)
import IrcTransport
import Markov
import Network.HTTP.Simple
import qualified Sqlite.EntityPersistence as SEP
import System.IO
import Text.InterpolatedString.QM
import Text.Printf

data BotState = BotState
  { bsConfig :: TwitchParams
  , bsSqliteConn :: SQLite.Connection
  , bsTimeouts :: [(Integer, Effect ())]
  , bsIncoming :: IncomingQueue
  , bsOutcoming :: OutcomingQueue
  , bsMarkov :: Maybe Markov
  }

newBotState :: Maybe Markov -> TwitchParams -> SQLite.Connection -> IO BotState
newBotState markov conf sqliteConn = do
  incoming <- atomically newTQueue
  outcoming <- atomically newTQueue
  return
    BotState
      { bsConfig = conf
      , bsSqliteConn = sqliteConn
      , bsTimeouts = []
      , bsIncoming = incoming
      , bsOutcoming = outcoming
      , bsMarkov = markov
      }

withBotState' ::
     Maybe Markov -> TwitchParams -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState' markov conf databasePath block =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    newBotState markov conf sqliteConn >>= block

withBotState ::
     Maybe FilePath -> FilePath -> FilePath -> (BotState -> IO ()) -> IO ()
withBotState markovPath tcPath databasePath block = do
  conf <- configFromFile tcPath
  case conf of
    TwitchConfig twitchParams -> do
      markov <- runMaybeT (MaybeT (return markovPath) >>= lift . loadMarkov)
      withBotState' markov twitchParams databasePath block
    -- TODO(#451): Discord bot instance is not supported
    DiscordConfig _ -> error "Discord bot instance is not supported"

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

applyEffect :: (BotState, Effect ()) -> IO (BotState, Effect ())
applyEffect self@(_, Pure _) = return self
applyEffect (botState, Free (Say text s)) = do
  atomically $
    writeTQueue (bsOutcoming botState) $
    ircPrivmsg (tpChannel $ bsConfig botState) $ twitchCmdEscape text
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
  let clientId = fromString $ T.unpack $ tpClientId $ bsConfig botState
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
      (tpChannel $ bsConfig botState)
      [qms|/{name} {T.concat $ intersperse " " args}|]
  return (botState, s)
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
listenEffectIO f (x, Free (Say text s)) = do
  (x', sayLog) <- listenEffectIO f (x, s)
  return (x', text : sayLog)
listenEffectIO f effect = f effect >>= listenEffectIO f

runEffectTransIO :: BotState -> Effect () -> IO BotState
runEffectTransIO botState effect =
  SQLite.withTransaction (bsSqliteConn botState) $
  runEffectIO applyEffect (botState, effect)

joinChannel :: Bot -> BotState -> T.Text -> IO BotState
joinChannel b botState = runEffectTransIO botState . b . Joined

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
          , senderDisplayName = displayName
          , senderChannel = idText target
          , senderSubscriber = any (T.isPrefixOf "subscriber") badges
          , senderMod = any (T.isPrefixOf "moderator") badges
          , senderBroadcaster = any (T.isPrefixOf "broadcaster") badges
          , senderOwner = name == tpOwner (bsConfig botState)
          }
        msgText
      where name = idText $ userNick userInfo
            displayName =
              maybe name valueOfTag $
              find (\(TagEntry ident _) -> ident == "display-name") $
              _msgTags msg
    (Join userInfo _ _) -> joinChannel b botState $ idText $ userNick userInfo
    _ -> return botState
