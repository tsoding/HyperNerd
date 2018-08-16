{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Bot
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free
import           Data.List
import           Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T ( encodeUtf8 )
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
import qualified Sqlite.EntityPersistence as SEP
import           System.IO
import           System.Clock
import           System.Environment

-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32

data BotState =
    BotState { bsConfig :: Config
             , bsSqliteConn :: SQLite.Connection
             , bsTimeouts :: [(Integer, Effect ())]
             , bsIncoming :: IncomingQueue
             , bsOutcoming :: OutcomingQueue
             }

-- Effect Monad interpreter in IO

runIO :: forall a. Effect a -> BotState -> IO BotState

runIO (Pure _) s = return s

runIO (Free (Say text next)) s@BotState{..} =
  do atomically $ writeTQueue bsOutcoming $ ircPrivmsg (configChannel bsConfig) text
     runIO next s

runIO (Free (LogMsg text next)) s =
  do T.putStrLn text
     runIO next s

runIO (Free (ErrorEff text)) s =
  do T.hPutStrLn stderr $ T.append "[ERROR] " text
     return s

runIO (Free (CreateEntity name props next)) s@BotState{..} =
  do entityID <- SEP.createEntity bsSqliteConn name props
     runIO (next entityID) s

runIO (Free (GetEntityById name entityID next)) s@BotState{..} =
  do entity <- SEP.getEntityById bsSqliteConn name entityID
     runIO (next entity) s

runIO (Free (UpdateEntityById e next)) s@BotState{..} =
  do e' <- SEP.updateEntityById bsSqliteConn e
     runIO (next e') s

runIO (Free (SelectEntities name sel next)) s@BotState{..} =
  do es <- SEP.selectEntities bsSqliteConn name sel
     runIO (next es) s

runIO (Free (DeleteEntities name sel next)) s@BotState{..} =
  do n <- SEP.deleteEntities bsSqliteConn name sel
     runIO (next n) s

runIO (Free (UpdateEntities name sel props next)) s@BotState{..} =
  do n <- SEP.updateEntities bsSqliteConn name sel props
     runIO (next n) s

runIO (Free (Now next)) s =
  do t <- getCurrentTime
     runIO (next t) s

runIO (Free (HttpRequest req next)) s =
  do res <- httpLBS req
     runIO (next res) s

runIO (Free (TwitchApiRequest req next)) s@BotState{..} =
  do clientID <- return $ T.encodeUtf8 $ configClientId bsConfig
     res <- httpLBS (addRequestHeader "Client-ID" clientID req)
     runIO (next res) s

runIO (Free (Timeout dt effect next)) s@BotState{..} =
  do runIO next (s { bsTimeouts = (dt,effect) : bsTimeouts })


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
                (runIO (b Join) botState))

handleIrcMessage :: Bot -> BotState -> RawIrcMsg -> IO BotState
handleIrcMessage b botState msg =
    do cookedMsg <- return $ cookIrcMsg msg
       print cookedMsg
       case cookedMsg of
         (Ping xs) -> do atomically $ writeTQueue (bsOutcoming botState) (ircPong xs)
                         return botState
         (Privmsg userInfo target msgText) ->
             SQLite.withTransaction (bsSqliteConn botState)
               $ flip runIO botState (b $ Msg Sender { senderName = idText $ userNick userInfo
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
