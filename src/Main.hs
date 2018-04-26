module Main where

import           Bot
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Free
import           Data.Foldable
import qualified Data.Text as T
import           Data.Time
import qualified Database.SQLite.Simple as SQLite
import           Effect
import           Events()
import           Hookup
import           Irc.Commands ( ircPong
                              , ircPrivmsg
                              )
import           Irc.Identifier (idText)
import           Irc.Message (IrcMsg(Ping, Privmsg))
import           Irc.UserInfo (userNick)
import           IrcTransport
import           Network.HTTP.Simple
import qualified SqliteEntityPersistence as SEP
import           System.CPUTime
import           System.Environment

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data EffectState =
    EffectState { esConfig :: Config
                , esIrcConn :: Connection
                , esSqliteConn :: SQLite.Connection
                , esTimeouts :: [(Integer, Effect ())]
                }

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

advanceTimeouts :: Integer -> EffectState -> IO EffectState
advanceTimeouts dt effectState =
    foldl (\esIO e -> esIO >>= (flip applyEffect e))
          (return $ effectState { esTimeouts = unripe })
      $ map snd ripe
    where (ripe, unripe) = span ((< 0) . fst)
                             $ map (\(t, e) -> (t - dt, e))
                             $ esTimeouts effectState

ircTransport :: Bot -> EffectState -> IO ()
ircTransport b effectState =
    -- TODO(#17): check unsuccessful authorization
    do authorize (esConfig effectState) ircConn
       eventLoop b
         <$> (getCPUTime)
         <*> (SQLite.withTransaction sqliteConn
                $ applyEffect effectState
                $ b Join)
         >>= id
    where ircConn = esIrcConn effectState
          sqliteConn = esSqliteConn effectState


handleIrcMessage :: Bot -> IrcMsg -> EffectState -> IO EffectState
handleIrcMessage _ (Ping xs) effectState =
    do sendMsg (esIrcConn effectState) (ircPong xs)
       return effectState
handleIrcMessage b (Privmsg userInfo target msgText) effectState =
    SQLite.withTransaction (esSqliteConn effectState)
    $ applyEffect effectState (b $ Msg (Sender { senderName = idText $ userNick $ userInfo
                                               , senderChannel = idText $ target
                                               })
                                       msgText)
handleIrcMessage _ _ effectState = return effectState

eventLoop :: Bot -> Integer -> EffectState -> IO ()
eventLoop b prevCPUTime effectState =
    do mb <- readIrcLine ircConn
       for_ mb $ \msg ->
           do print msg
              currCPUTime <- getCPUTime
              let deltaTime = (currCPUTime - prevCPUTime) `div` ((10 :: Integer) ^ (9 :: Integer))
              handleIrcMessage b msg effectState
                >>= advanceTimeouts deltaTime
                >>= eventLoop b currCPUTime
    where ircConn = esIrcConn effectState

singleThreadedMain :: FilePath -> FilePath -> IO ()
singleThreadedMain configPath databasePath =
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

-- TODO(#105): Main.logicEntry is not implemented
logicEntry :: IncomingQueue -> OutcomingQueue -> FilePath -> IO ()
logicEntry incoming outcoming configPath =
    do msg <- atomically $ tryReadTQueue incoming
       maybe (return ()) print msg
       logicEntry incoming outcoming configPath

multiThreadedMain :: FilePath -> FilePath -> IO ()
multiThreadedMain configPath _ =
    do incoming <- atomically $ newTQueue
       outcoming <- atomically $ newTQueue
       _ <- forkIO $ ircTransportEntry incoming outcoming configPath
       logicEntry incoming outcoming configPath

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = singleThreadedMain configPath databasePath
mainWithArgs ["--mt", configPath, databasePath] = multiThreadedMain configPath databasePath
mainWithArgs _ = error "./HyperNerd [--mt] <config-file> <database-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
