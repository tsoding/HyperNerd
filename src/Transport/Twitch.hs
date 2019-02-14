{-# LANGUAGE OverloadedStrings #-}

module Transport.Twitch
  ( twitchTransportEntry
  , IncomingQueue
  , OutcomingQueue
  ) where

import Config
import Control.Concurrent.Async
import Control.Exception
import Data.Foldable
import Data.Traversable
import Hookup
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPass)
import Irc.RawIrcMsg (RawIrcMsg, asUtf8, parseRawIrcMsg, renderRawIrcMsg)
import Network.Socket (Family(..))
import Transport
import Control.Concurrent.STM
import Irc.Message (IrcMsg(Join, Ping, Privmsg), cookIrcMsg)
import Irc.Commands (ircPong, ircPrivmsg)
import qualified Data.Text as T
import Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import Irc.Identifier (idText)
import Data.Maybe
import Irc.UserInfo (userNick)

maxIrcMessage :: Int
maxIrcMessage = 1000

twitchConnectionParams :: ConnectionParams
twitchConnectionParams =
  ConnectionParams
    { cpHost = "irc.chat.twitch.tv"
    , cpPort = 443
    , cpTls =
        Just
          TlsParams
            { tpClientCertificate = Nothing
            , tpClientPrivateKey = Nothing
            , tpServerCertificate = Nothing
            , tpCipherSuite = "HIGH"
            , tpInsecure = False
            }
    , cpSocks = Nothing
    , cpFamily = AF_INET
    }

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

authorize :: TwitchParams -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ tpPass conf)
  sendMsg conn (ircNick $ tpNick conf)
  sendMsg conn (ircJoin (tpChannel conf) Nothing)
  sendMsg conn (ircCapReq ["twitch.tv/tags"])

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine conn = do
  mb <- recvLine conn maxIrcMessage
  for mb $ \xs ->
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! msg
      Nothing -> fail "Server sent invalid message!"

valueOfTag :: TagEntry -> T.Text
valueOfTag (TagEntry _ value) = value

receiveLoop :: T.Text -> IncomingQueue -> Connection -> IO ()
receiveLoop owner incoming ircConn = do
  mb <- readIrcLine ircConn
  for_ mb $ \msg -> do
    let badges =
          concat $
          maybeToList $
          fmap (T.splitOn "," . valueOfTag) $
          find (\(TagEntry ident _) -> ident == "badges") $ _msgTags msg
    let cookedMsg = cookIrcMsg msg
    print cookedMsg
    case cookedMsg of
      (Ping xs) -> do
        sendMsg ircConn (ircPong xs)
      (Privmsg userInfo target msgText) ->
        atomically $
        writeTQueue incoming $
        InMsg
          Sender
            { senderName = name
            , senderDisplayName = displayName
            , senderChannel = idText target
            , senderSubscriber = any (T.isPrefixOf "subscriber") badges
            , senderMod = any (T.isPrefixOf "moderator") badges
            , senderBroadcaster = any (T.isPrefixOf "broadcaster") badges
            , senderOwner = name == owner
            }
          msgText
        where name = idText $ userNick userInfo
              displayName =
                maybe name valueOfTag $
                find (\(TagEntry ident _) -> ident == "display-name") $
                _msgTags msg
      (Join userInfo _ _) ->
        atomically $ writeTQueue incoming $ Joined $ idText $ userNick userInfo
      _ -> return ()
  receiveLoop owner incoming ircConn

sendLoop :: T.Text -> OutcomingQueue -> Connection -> IO ()
sendLoop channel outcoming ircConn = do
  outMsg <- atomically $ readTQueue outcoming
  case outMsg of
    OutMsg text -> sendMsg ircConn $ ircPrivmsg channel text
  sendLoop channel outcoming ircConn

-- TODO(#17): check unsuccessful authorization
twitchTransportEntry :: IncomingQueue -> OutcomingQueue -> TwitchParams -> IO ()
twitchTransportEntry incoming outcoming conf = do
  withConnection twitchConnectionParams $ \ircConn -> do
    authorize conf ircConn
    withAsync (sendLoop (tpChannel conf) outcoming ircConn) $ \sender ->
      withAsync (receiveLoop (tpOwner conf) incoming ircConn) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32
