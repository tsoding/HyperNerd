{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Bot
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free
import           Data.List
import           Data.String
import qualified Data.Text as T
import           Data.Time
import qualified Database.SQLite.Simple as SQLite
import           Effect
import           Irc.Commands ( ircPong
                              , ircPrivmsg
                              )
import           Irc.Identifier (idText)
import           Irc.Message (IrcMsg(Ping, Privmsg), cookIrcMsg)
import           Irc.RawIrcMsg (RawIrcMsg(..), TagEntry(..))
import           Irc.UserInfo (userNick)
import           IrcTransport
import           Network.HTTP.Simple
import qualified SqliteEntityPersistence as SEP
import           System.CPUTime
import           System.Environment
import           Text.Printf

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data EffectState =
    EffectState { esConfig :: Config
                , esSqliteConn :: SQLite.Connection
                , esTimeouts :: [(Integer, Effect ())]
                , esIncoming :: IncomingQueue
                , esOutcoming :: OutcomingQueue
                }

applyEffect :: EffectState -> Effect () -> IO EffectState
applyEffect effectState (Pure _) = return effectState
applyEffect effectState (Free (Say text s)) =
    do atomically $ writeTQueue (esOutcoming effectState) (ircPrivmsg (configChannel $ esConfig effectState) text)
       applyEffect effectState s
applyEffect effectState (Free (LogMsg msg s)) =
    do putStrLn $ T.unpack msg
       applyEffect effectState s
applyEffect effectState (Free (Now s)) =
    do timestamp <- getCurrentTime
       applyEffect effectState (s timestamp)
applyEffect effectState (Free (ErrorEff msg)) =
    do putStrLn $ printf "[ERROR] %s" msg
       return effectState

applyEffect effectState (Free (CreateEntity name properties s)) =
    do entityId <- SEP.createEntity (esSqliteConn effectState) name properties
       applyEffect effectState (s entityId)
applyEffect effectState (Free (GetEntityById name entityId s)) =
    do entity <- SEP.getEntityById (esSqliteConn effectState) name entityId
       applyEffect effectState (s entity)
applyEffect effectState (Free (GetRandomEntity name selector s)) =
    do entity <- SEP.getRandomEntity (esSqliteConn effectState) name selector
       applyEffect effectState (s entity)
applyEffect effectState (Free (SelectEntities name selector s)) =
    do entities <- SEP.selectEntities (esSqliteConn effectState) name selector
       applyEffect effectState (s entities)
applyEffect effectState (Free (HttpRequest request s)) =
    do response <- httpLBS request
       applyEffect effectState (s response)
applyEffect effectState (Free (TwitchApiRequest request s)) =
    do clientId <- return $ fromString $ T.unpack $ configClientId $ esConfig effectState
       response <- httpLBS (addRequestHeader "Client-ID" clientId request)
       applyEffect effectState (s response)
applyEffect effectState (Free (Timeout ms e s)) =
    applyEffect (effectState { esTimeouts = (ms, e) : esTimeouts effectState }) s

advanceTimeouts :: Integer -> EffectState -> IO EffectState
advanceTimeouts dt effectState =
    foldl (\esIO e -> esIO >>= flip applyEffect e)
          (return $ effectState { esTimeouts = unripe })
      $ map snd ripe
    where (ripe, unripe) = span ((< 0) . fst)
                             $ map (\(t, e) -> (t - dt, e))
                             $ esTimeouts effectState

ircTransport :: Bot -> EffectState -> IO ()
ircTransport b effectState =
    join
      (eventLoop b
          <$> getCPUTime
          <*> SQLite.withTransaction (esSqliteConn effectState)
                (applyEffect effectState $ b Join))

handleIrcMessage :: Bot -> EffectState -> RawIrcMsg -> IO EffectState
handleIrcMessage b effectState msg =
    case cookIrcMsg msg of
      (Ping xs) -> do atomically $ writeTQueue (esOutcoming effectState) (ircPong xs)
                      return effectState
      (Privmsg userInfo target msgText) ->
          SQLite.withTransaction (esSqliteConn effectState)
            $ applyEffect effectState (b $ Msg Sender { senderName = idText $ userNick userInfo
                                                      , senderChannel = idText target
                                                      , senderSubscriber = maybe False (\(TagEntry _ value) -> value == "1")
                                                                             $ find (\(TagEntry ident _) -> ident == "subscriber")
                                                                             $ _msgTags msg
                                                      }
                                       msgText)
      _ -> return effectState

eventLoop :: Bot -> Integer -> EffectState -> IO ()
eventLoop b prevCPUTime effectState =
    do threadDelay 10000        -- to prevent busy looping
       currCPUTime <- getCPUTime
       let deltaTime = (currCPUTime - prevCPUTime) `div` ((10 :: Integer) ^ (9 :: Integer))
       mb <- atomically $ tryReadTQueue (esIncoming effectState)
       maybe (return effectState)
             (\msg -> do print msg
                         handleIrcMessage b effectState msg)
             mb
         >>= advanceTimeouts deltaTime
         >>= eventLoop b currCPUTime

logicEntry :: IncomingQueue -> OutcomingQueue -> Config -> String -> IO ()
logicEntry incoming outcoming conf databasePath =
    SQLite.withConnection databasePath
      $ \sqliteConn -> do SEP.prepareSchema sqliteConn
                          ircTransport bot
                             EffectState { esConfig = conf
                                         , esSqliteConn = sqliteConn
                                         , esTimeouts = []
                                         , esIncoming = incoming
                                         , esOutcoming = outcoming
                                         }

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] =
    do incoming <- atomically newTQueue
       outcoming <- atomically newTQueue
       conf <- configFromFile configPath
       _ <- forkIO $ ircTransportEntry incoming outcoming configPath
       logicEntry incoming outcoming conf databasePath
mainWithArgs _ = error "./HyperNerd <config-file> <database-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
