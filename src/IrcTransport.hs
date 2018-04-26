module IrcTransport where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.Ini
import qualified Data.Text as T
import           Data.Traversable
import           Hookup
import           Irc.Commands ( ircPass
                              , ircJoin
                              , ircNick
                              )
import           Irc.Message (IrcMsg)
import           Irc.Message (cookIrcMsg)
import           Irc.RawIrcMsg (RawIrcMsg)
import           Irc.RawIrcMsg (parseRawIrcMsg, asUtf8, renderRawIrcMsg)
import           Text.Printf

type IncomingQueue = TQueue IrcMsg
type OutcomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     , configChannel :: T.Text
                     } deriving Show

maxIrcMessage :: Int
maxIrcMessage = 512

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

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params body =
    bracket (connect params) close body

config :: T.Text -> T.Text -> T.Text -> Config
config nick password channel =
    Config { configNick = nick
           , configPass = password
           , configChannel = T.pack $ printf "#%s" channel
           }

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       let lookupParam section key = ini >>= lookupValue (T.pack section) (T.pack key)
       let nick = lookupParam "User" "nick"
       let password = lookupParam "User" "password"
       let channel = lookupParam "User" "channel"
       either (ioError . userError) return $ liftM3 config nick password channel

readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine conn =
    do mb <- recvLine conn maxIrcMessage
       for mb $ \xs ->
           case parseRawIrcMsg (asUtf8 xs) of
             Just msg -> return $! cookIrcMsg msg
             Nothing -> fail "Server sent invalid message!"

eventLoop' :: IncomingQueue -> OutcomingQueue -> Connection -> IO ()
eventLoop' incoming outcoming ircConn =
    do mb <- readIrcLine ircConn
       for_ mb $ \msg -> atomically $ writeTQueue incoming msg
       outMsg <- atomically $ tryReadTQueue outcoming
       maybe (return ()) (sendMsg ircConn) outMsg
       eventLoop' incoming outcoming ircConn

-- TODO(#104): IrcTransport.ircTransportEntry is not implemented
ircTransportEntry :: IncomingQueue -> OutcomingQueue -> FilePath -> IO ()
ircTransportEntry incoming outcoming configFilePath =
    do conf <- configFromFile configFilePath
       withConnection twitchConnectionParams $ \ircConn ->
           do authorize conf ircConn
              eventLoop' incoming outcoming ircConn
       return ()
