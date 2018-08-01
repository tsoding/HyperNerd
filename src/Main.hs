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
import           System.Clock
import           System.Environment
import           Text.Printf

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data BotState =
    BotState { bsConfig :: Config
             , bsSqliteConn :: SQLite.Connection
             , bsTimeouts :: [(Integer, Effect ())]
             , bsIncoming :: IncomingQueue
             , bsOutcoming :: OutcomingQueue
             }

applyEffect :: BotState -> Effect () -> IO BotState
applyEffect botState (Pure _) = return botState
applyEffect botState (Free (Say text s)) =
    do atomically $ writeTQueue (bsOutcoming botState) (ircPrivmsg (configChannel $ bsConfig botState) text)
       applyEffect botState s
applyEffect botState (Free (LogMsg msg s)) =
    do putStrLn $ T.unpack msg
       applyEffect botState s
applyEffect botState (Free (Now s)) =
    do timestamp <- getCurrentTime
       applyEffect botState (s timestamp)
applyEffect botState (Free (ErrorEff msg)) =
    do putStrLn $ printf "[ERROR] %s" msg
       return botState

applyEffect botState (Free (CreateEntity name properties s)) =
    do entityId <- SEP.createEntity (bsSqliteConn botState) name properties
       applyEffect botState (s entityId)
applyEffect botState (Free (GetEntityById name entityId s)) =
    do entity <- SEP.getEntityById (bsSqliteConn botState) name entityId
       applyEffect botState (s entity)
applyEffect botState (Free (SelectEntities name selector s)) =
    do entities <- SEP.selectEntities (bsSqliteConn botState) name selector
       applyEffect botState (s entities)
applyEffect botState (Free (DeleteEntities name selector s)) =
    do n <- SEP.deleteEntities (bsSqliteConn botState) name selector
       applyEffect botState (s n)
applyEffect botState (Free (UpdateEntities name selector action s)) =
    do n <- SEP.updateEntities (bsSqliteConn botState) name selector action
       applyEffect botState (s n)
applyEffect botState (Free (HttpRequest request s)) =
    do response <- httpLBS request
       applyEffect botState (s response)
applyEffect botState (Free (TwitchApiRequest request s)) =
    do clientId <- return $ fromString $ T.unpack $ configClientId $ bsConfig botState
       response <- httpLBS (addRequestHeader "Client-ID" clientId request)
       applyEffect botState (s response)
applyEffect botState (Free (Timeout ms e s)) =
    applyEffect (botState { bsTimeouts = (ms, e) : bsTimeouts botState }) s

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
    foldl (\esIO e -> esIO >>= flip applyEffect e)
          (return $ botState { bsTimeouts = unripe })
      $ map snd ripe
    where (ripe, unripe) = span ((< 0) . fst)
                             $ map (\(t, e) -> (t - dt, e))
                             $ bsTimeouts botState

ircTransport :: Bot -> BotState -> IO ()
ircTransport b botState =
    join
      (eventLoop b
          <$> getTime Monotonic
          <*> SQLite.withTransaction (bsSqliteConn botState)
                (applyEffect botState $ b Join))

handleIrcMessage :: Bot -> BotState -> RawIrcMsg -> IO BotState
handleIrcMessage b botState msg =
    do cookedMsg <- return $ cookIrcMsg msg
       print cookedMsg
       case cookedMsg of
         (Ping xs) -> do atomically $ writeTQueue (bsOutcoming botState) (ircPong xs)
                         return botState
         (Privmsg userInfo target msgText) ->
             SQLite.withTransaction (bsSqliteConn botState)
               $ applyEffect botState (b $ Msg Sender { senderName = idText $ userNick userInfo
                                                      , senderChannel = idText target
                                                      , senderSubscriber = maybe False (\(TagEntry _ value) -> value == "1")
                                                                             $ find (\(TagEntry ident _) -> ident == "subscriber")
                                                                             $ _msgTags msg
                                                      , senderMod = maybe False (\(TagEntry _ value) -> value == "1")
                                                                      $ find (\(TagEntry ident _) -> ident == "mod")
                                                                      $ _msgTags msg
                                                      }
                                          msgText)
         _ -> return botState

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState =
    do threadDelay 10000        -- to prevent busy looping
       currCPUTime <- getTime Monotonic
       let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
       mb <- atomically $ tryReadTQueue (bsIncoming botState)
       maybe (return botState)
             (handleIrcMessage b botState)
             mb
         >>= advanceTimeouts deltaTime
         >>= eventLoop b currCPUTime

logicEntry :: IncomingQueue -> OutcomingQueue -> Config -> String -> IO ()
logicEntry incoming outcoming conf databasePath =
    SQLite.withConnection databasePath
      $ \sqliteConn -> do SEP.prepareSchema sqliteConn
                          ircTransport bot
                             BotState { bsConfig = conf
                                      , bsSqliteConn = sqliteConn
                                      , bsTimeouts = []
                                      , bsIncoming = incoming
                                      , bsOutcoming = outcoming
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
