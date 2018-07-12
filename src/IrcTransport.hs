{-# LANGUAGE OverloadedStrings #-}
module IrcTransport where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Data.Foldable
import           Data.Ini
import qualified Data.Text as T
import           Data.Traversable
import           Hookup
import           Irc.Commands ( ircPass
                              , ircJoin
                              , ircNick
                              , ircCapReq
                              )
import           Irc.RawIrcMsg (RawIrcMsg, parseRawIrcMsg, asUtf8, renderRawIrcMsg)
import           Text.Printf

type IncomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg
type OutcomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     , configChannel :: T.Text
                     , configClientId :: T.Text
                     } deriving Show

maxIrcMessage :: Int
maxIrcMessage = 1000

twitchConnectionParams :: ConnectionParams
twitchConnectionParams =
    ConnectionParams { cpHost = "irc.chat.twitch.tv"
                     , cpPort = 443
                     , cpTls = Just TlsParams { tpClientCertificate = Nothing
                                              , tpClientPrivateKey = Nothing
                                              , tpServerCertificate = Nothing
                                              , tpCipherSuite = "HIGH"
                                              , tpInsecure = False
                                              }
                     , cpSocks = Nothing
                     }

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

authorize :: Config -> Connection -> IO ()
authorize conf conn =
    do sendMsg conn (ircPass $ configPass conf)
       sendMsg conn (ircNick $ configNick conf)
       sendMsg conn (ircJoin (configChannel conf) Nothing)
       sendMsg conn (ircCapReq ["twitch.tv/tags"])

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       either (ioError . userError) return $
         do nick     <- ini >>= lookupValue "User" "nick"
            password <- ini >>= lookupValue "User" "password"
            channel  <- ini >>= lookupValue "User" "channel"
            clientId <- ini >>= lookupValue "User" "clientId"
            return Config { configNick = nick
                          , configPass = password
                          , configChannel = T.pack $ printf "#%s" channel
                          , configClientId = clientId
                          }

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine conn =
    do mb <- recvLine conn maxIrcMessage
       for mb $ \xs ->
           case parseRawIrcMsg (asUtf8 xs) of
             Just msg -> return $! msg
             Nothing -> fail "Server sent invalid message!"


receiveLoop :: IncomingQueue -> Connection -> IO ()
receiveLoop incoming ircConn =
    do mb <- readIrcLine ircConn
       for_ mb $ \msg -> atomically $ writeTQueue incoming msg
       receiveLoop incoming ircConn

sendLoop :: OutcomingQueue -> Connection -> IO ()
sendLoop outcoming ircConn =
    do outMsg <- atomically $ readTQueue outcoming
       sendMsg ircConn outMsg
       sendLoop outcoming ircConn

ircTransportEntry :: IncomingQueue -> OutcomingQueue -> FilePath -> IO ()
ircTransportEntry incoming outcoming configFilePath =
    do conf <- configFromFile configFilePath
       withConnection twitchConnectionParams $ \ircConn ->
           -- TODO(#17): check unsuccessful authorization
           do authorize conf ircConn
              withAsync (sendLoop outcoming ircConn) $ \sender ->
                  withAsync (receiveLoop incoming ircConn) $ \receive ->
                      do res <- waitEitherCatch sender receive
                         case res of
                           Left  Right{}  -> fail "PANIC: sendLoop returned"
                           Right Right{}  -> return ()
                           Left  (Left e) -> throwIO e
                           Right (Left e) -> throwIO e
       return ()
