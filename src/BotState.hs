{-# LANGUAGE OverloadedStrings #-}
module BotState where

import           Bot
import           Control.Concurrent.STM
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
import qualified Sqlite.EntityPersistence as SEP
import           Text.Printf

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
applyEffect botState (Free (DeleteEntityById name entityId s)) =
    do SEP.deleteEntityById (bsSqliteConn botState) name entityId
       applyEffect botState s
applyEffect botState (Free (UpdateEntityById entity s)) =
    do entity' <- SEP.updateEntityById (bsSqliteConn botState) entity
       applyEffect botState (s entity')
applyEffect botState (Free (SelectEntities name selector s)) =
    do entities <- SEP.selectEntities (bsSqliteConn botState) name selector
       applyEffect botState (s entities)
applyEffect botState (Free (DeleteEntities name selector s)) =
    do n <- SEP.deleteEntities (bsSqliteConn botState) name selector
       applyEffect botState (s n)
applyEffect botState (Free (UpdateEntities name selector properties s)) =
    do n <- SEP.updateEntities (bsSqliteConn botState) name selector properties
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
-- TODO(#224): RedirectSay effect is not interpreted
applyEffect botState (Free (RedirectSay _ s)) =
    applyEffect botState (s [])

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
    foldl (\esIO e -> esIO >>= flip applyEffect e)
          (return $ botState { bsTimeouts = unripe })
      $ map snd ripe
    where (ripe, unripe) = span ((< 0) . fst)
                             $ map (\(t, e) -> (t - dt, e))
                             $ bsTimeouts botState

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
