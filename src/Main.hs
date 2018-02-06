module Main where

import Bot
import Control.Exception
import Control.Monad
import Control.Monad.Free
import Data.Foldable
import Data.Ini
import qualified Data.Text as T
import Data.Traversable
import Effect
import Hookup
import Irc.Commands (ircPong, ircNick, ircPass, ircJoin, ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Ping, Privmsg), cookIrcMsg)
import Irc.RateLimit (RateLimit)
import Irc.RawIrcMsg (RawIrcMsg, parseRawIrcMsg, asUtf8, renderRawIrcMsg)
import Irc.UserInfo (userNick)
import System.Environment

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     , configChannel :: T.Text
                     } deriving Show

maxIrcMessage :: Int
maxIrcMessage = 512

config :: T.Text -> T.Text -> T.Text -> Config
config nick password channel =
    Config { configNick = nick
           , configPass = password
           , configChannel = T.concat [(T.pack "#"), channel]
           }

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       let lookup section key = ini >>= lookupValue (T.pack section) (T.pack key)
       let nick = lookup "User" "nick"
       let password = lookup "User" "password"
       let channel = lookup "User" "channel"
       either (ioError . userError) return $ liftM3 config nick password channel

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
       sendMsg conn (ircJoin (configChannel config) Nothing)

readIrcLine :: Connection -> IO (Maybe IrcMsg)
readIrcLine conn =
    do mb <- recvLine conn maxIrcMessage
       for mb $ \xs ->
           case parseRawIrcMsg (asUtf8 xs) of
             Just msg -> return $! cookIrcMsg msg
             Nothing -> fail "Server sent invalid message!"

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

applyEffect :: Config -> Connection -> Effect () -> IO ()
applyEffect _ _ (Pure r) = return r
applyEffect config conn (Free (Ok s)) =
    applyEffect config conn s
applyEffect config conn (Free (Say text s)) =
    do sendMsg conn (ircPrivmsg (configChannel config) text)
       applyEffect config conn s

ircTransport :: Bot -> Config -> Connection -> IO ()
ircTransport bot config conn =
    -- TODO(#17): check unsuccessful authorization
    do authorize config conn
       applyEffect config conn $ bot Join
       eventLoop bot config conn

eventLoop :: Bot -> Config -> Connection -> IO ()
eventLoop bot config conn =
    do mb <- readIrcLine conn
       for_ mb $ \msg ->
           do print msg
              case msg of
                Ping xs -> sendMsg conn (ircPong xs)
                Privmsg userInfo _ msg -> applyEffect config conn (bot $ Msg (idText $ userNick $ userInfo) msg)
                _ -> return ()
              eventLoop bot config conn

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath] =
    do config <- configFromFile configPath
       withConnection twitchConnectionParams $ ircTransport bot config
mainWithArgs _ = error "./HyperNerd <config-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
