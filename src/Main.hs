module Main where

import Bot
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Ini
import qualified Data.Text as T
import Data.Traversable
import Hookup
import Irc.Commands (ircPong, ircNick, ircPass, ircJoin, ircPrivmsg)
import Irc.Message (IrcMsg(Ping), cookIrcMsg)
import Irc.RateLimit (RateLimit)
import Irc.RawIrcMsg (RawIrcMsg, parseRawIrcMsg, asUtf8, renderRawIrcMsg)
import System.Environment

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

-- TODO(#16): add channel to the config
data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     } deriving Show

maxIrcMessage :: Int
maxIrcMessage = 512

config :: T.Text -> T.Text -> Config
config nick password = Config { configNick = nick
                              , configPass = password
                              }

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       let lookup section key = ini >>= lookupValue (T.pack section) (T.pack key)
       let nick = lookup "User" "nick"
       let password = lookup "User" "password"
       either (ioError . userError) return $ liftM2 config nick password

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

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params body =
    bracket (connect params) close body

authorize :: Config -> Connection -> IO ()
authorize config conn =
    do sendMsg conn (ircPass $ configPass config)
       sendMsg conn (ircNick $ configNick config)
       sendMsg conn (ircJoin (T.pack "#tsoding") Nothing)

readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine conn =
    do mb <- recvLine conn maxIrcMessage
       for mb $ \xs ->
           case parseRawIrcMsg (asUtf8 xs) of
             Just msg -> return $! cookIrcMsg msg
             Nothing -> fail "Server sent invalid message!"

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

applyEffect :: Bot s -> Connection -> Effect s -> IO ()
applyEffect _ conn (Say text) =
    sendMsg conn (ircPrivmsg (T.pack "#tsoding") text)

ircTransport :: Bot s -> Config -> Connection -> IO ()
ircTransport bot config conn =
    -- TODO(#17): check unsuccessful authorization
    do authorize config conn
       applyEffect bot conn $ bot Join
       eventLoop bot conn

eventLoop :: Bot s -> Connection -> IO ()
eventLoop bot conn =
    do mb <- readIrcLine conn
       for_ mb $ \msg ->
           do print msg
              case msg of
                Ping xs -> sendMsg conn (ircPong xs)
                _ -> return ()
              eventLoop bot conn

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath] =
    do config <- configFromFile configPath
       withConnection twitchConnectionParams $ ircTransport bot config
mainWithArgs _ = error "./HyperNerd <config-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
