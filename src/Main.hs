module Main where

import Bot
import Control.Exception
import Control.Monad
import Control.Monad.Free
import Data.Foldable
import Data.Ini
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Effect
import Entity
import Hookup
import Irc.Commands (ircPong, ircNick, ircPass, ircJoin, ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Ping, Privmsg), cookIrcMsg)
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
       let lookupParam section key = ini >>= lookupValue (T.pack section) (T.pack key)
       let nick = lookupParam "User" "nick"
       let password = lookupParam "User" "password"
       let channel = lookupParam "User" "channel"
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
authorize conf conn =
    do sendMsg conn (ircPass $ configPass conf)
       sendMsg conn (ircNick $ configNick conf)
       sendMsg conn (ircJoin (configChannel conf) Nothing)

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
applyEffect conf conn (Free (Say text s)) =
    do sendMsg conn (ircPrivmsg (configChannel conf) text)
       applyEffect conf conn s

applyEffect conf conn (Free (Now s)) =
    do timestamp <- getCurrentTime
       applyEffect conf conn (s timestamp)

-- TODO(#37): Implement SaveEntity and GetEntityById effects
applyEffect conf conn (Free (SaveEntity _ s)) =
    applyEffect conf conn (s 42)
applyEffect conf conn (Free (GetEntityById name _ s)) =
    applyEffect conf conn (s $ Just $ Entity { entityName = name
                                             , entityProperties = M.empty
                                             })

ircTransport :: Bot -> Config -> Connection -> IO ()
ircTransport b conf conn =
    -- TODO(#17): check unsuccessful authorization
    do authorize conf conn
       applyEffect conf conn $ b Join
       eventLoop b conf conn

eventLoop :: Bot -> Config -> Connection -> IO ()
eventLoop b conf conn =
    do mb <- readIrcLine conn
       for_ mb $ \msg ->
           do print msg
              case msg of
                Ping xs -> sendMsg conn (ircPong xs)
                Privmsg userInfo _ msgText -> applyEffect conf conn (b $ Msg (idText $ userNick $ userInfo) msgText)
                _ -> return ()
              eventLoop b conf conn

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath] =
    do conf <- configFromFile configPath
       withConnection twitchConnectionParams $ ircTransport bot conf
mainWithArgs _ = error "./HyperNerd <config-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
