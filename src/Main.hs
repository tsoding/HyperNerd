module Main where

import           Bot
import           Control.Exception
import           Control.Monad
import           Control.Monad.Free
import           Data.Foldable
import           Data.Ini
import qualified Data.Text as T
import           Data.Time
import           Data.Traversable
import qualified Database.SQLite.Simple as SQLite
import           Effect
import           Hookup
import           Irc.Commands ( ircPong
                              , ircNick
                              , ircPass
                              , ircJoin
                              , ircPrivmsg
                              )
import           Irc.Identifier (idText)
import           Irc.Message (IrcMsg(Ping, Privmsg), cookIrcMsg)
import           Irc.RawIrcMsg (RawIrcMsg, parseRawIrcMsg, asUtf8, renderRawIrcMsg)
import           Irc.UserInfo (userNick)
import           Network.HTTP.Simple
import qualified SqliteEntityPersistence as SEP
import           System.Environment
import           Text.Printf

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     , configChannel :: T.Text
                     } deriving Show

data EffectState =
    EffectState { esConfig :: Config
                , esIrcConn :: Connection
                , esSqliteConn :: SQLite.Connection
                , esTimeouts :: [(Int, Effect ())]
                }

maxIrcMessage :: Int
maxIrcMessage = 512

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

applyEffect :: EffectState -> Effect () -> IO EffectState
applyEffect effectState (Pure _) = return effectState
applyEffect effectState (Free (Say text s)) =
    do sendMsg (esIrcConn effectState) (ircPrivmsg (configChannel $ esConfig $ effectState) text)
       applyEffect effectState s
applyEffect effectState (Free (LogMsg msg s)) =
    do putStrLn $ T.unpack msg
       applyEffect effectState s
applyEffect effectState (Free (Now s)) =
    do timestamp <- getCurrentTime
       applyEffect effectState (s timestamp)

applyEffect effectState (Free (CreateEntity name properties s)) =
    do entityId <- SEP.createEntity (esSqliteConn effectState) name properties
       applyEffect effectState (s entityId)
applyEffect effectState (Free (GetEntityById name entityId s)) =
    do entity <- SEP.getEntityById (esSqliteConn effectState) name entityId
       applyEffect effectState (s entity)
applyEffect effectState (Free (GetRandomEntity name s)) =
    do entity <- SEP.getRandomEntity (esSqliteConn effectState) name
       applyEffect effectState (s entity)
applyEffect effectState (Free (HttpRequest request s)) =
    do response <- httpLBS request
       applyEffect effectState (s response)
applyEffect effectState (Free (Timeout ms e s)) =
    applyEffect (effectState { esTimeouts = (ms, e) : esTimeouts effectState }) s

ircTransport :: Bot -> EffectState -> IO ()
ircTransport b effectState =
    -- TODO(#17): check unsuccessful authorization
    do authorize (esConfig effectState) ircConn
       (SQLite.withTransaction sqliteConn
        $ applyEffect effectState
        $ b Join)
         >>= eventLoop b
    where ircConn = esIrcConn effectState
          sqliteConn = esSqliteConn effectState


eventLoop :: Bot -> EffectState -> IO ()
eventLoop b effectState =
    do mb <- readIrcLine ircConn
       for_ mb $ \msg ->
           do print msg
              case msg of
                Ping xs ->
                    do sendMsg ircConn (ircPong xs)
                       eventLoop b effectState
                Privmsg userInfo _ msgText ->
                    do effectState' <- SQLite.withTransaction sqliteConn
                                         $ applyEffect effectState (b $ Msg (idText $ userNick $ userInfo) msgText)
                       eventLoop b effectState'
                _ -> eventLoop b effectState
    where ircConn = esIrcConn effectState
          sqliteConn = esSqliteConn effectState

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] =
    do conf <- configFromFile configPath
       withConnection twitchConnectionParams
         $ \ircConn -> SQLite.withConnection databasePath
         $ \sqliteConn -> do SEP.prepareSchema sqliteConn
                             ircTransport bot
                               $ EffectState { esConfig = conf
                                             , esIrcConn = ircConn
                                             , esSqliteConn = sqliteConn
                                             , esTimeouts = []
                                             }
mainWithArgs _ = error "./HyperNerd <config-file> <database-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
