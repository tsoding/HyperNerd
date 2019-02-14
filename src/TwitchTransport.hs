{-# LANGUAGE OverloadedStrings #-}

module TwitchTransport
  ( twitchTransportEntry
  , IncomingQueue
  , OutcomingQueue
  ) where

import Config
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable
import Data.Traversable
import Hookup
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPass)
import Irc.RawIrcMsg (RawIrcMsg, asUtf8, parseRawIrcMsg, renderRawIrcMsg)
import Network.Socket (Family(..))

type IncomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg

type OutcomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg

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

receiveLoop :: IncomingQueue -> Connection -> IO ()
receiveLoop incoming ircConn = do
  mb <- readIrcLine ircConn
  for_ mb $ \msg -> atomically $ writeTQueue incoming msg
  receiveLoop incoming ircConn

sendLoop :: OutcomingQueue -> Connection -> IO ()
sendLoop outcoming ircConn = do
  outMsg <- atomically $ readTQueue outcoming
  sendMsg ircConn outMsg
  sendLoop outcoming ircConn

-- TODO(#17): check unsuccessful authorization
twitchTransportEntry :: IncomingQueue -> OutcomingQueue -> TwitchParams -> IO ()
twitchTransportEntry incoming outcoming conf = do
  withConnection twitchConnectionParams $ \ircConn -> do
    authorize conf ircConn
    withAsync (sendLoop outcoming ircConn) $ \sender ->
      withAsync (receiveLoop incoming ircConn) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32
