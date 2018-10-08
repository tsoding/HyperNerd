{-# LANGUAGE OverloadedStrings #-}
module BotState ( joinChannel
                , advanceTimeouts
                , handleIrcMessage
                , BotState (..)
                ) where

import           Bot
import           Config
import           Control.Concurrent.STM
import           Control.Monad.Free
import           Data.Foldable
import           Data.Maybe
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
    do atomically $
         writeTQueue (bsOutcoming botState) $
         ircPrivmsg (configChannel $ bsConfig botState) text
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
    do let clientId = fromString $ T.unpack $ configClientId $ bsConfig botState
       response <- httpLBS (addRequestHeader "Client-ID" clientId request)
       applyEffect botState (s response)
applyEffect botState (Free (Timeout ms e s)) =
    applyEffect (botState { bsTimeouts = (ms, e) : bsTimeouts botState }) s
-- TODO(#224): RedirectSay effect is not interpreted
applyEffect botState (Free (RedirectSay _ s)) =
    applyEffect botState (s [])

applyEffectInTransaction :: BotState -> Effect () -> IO BotState
applyEffectInTransaction botState =
    SQLite.withTransaction (bsSqliteConn botState) . applyEffect botState

joinChannel :: Bot -> BotState -> IO BotState
joinChannel b botState = applyEffectInTransaction botState $ b Join

advanceTimeouts :: Integer -> BotState -> IO BotState
advanceTimeouts dt botState =
    foldlM applyEffectInTransaction (botState { bsTimeouts = unripe }) $
    map snd ripe
    where (ripe, unripe) = span ((< 0) . fst) $
                           map (\(t, e) -> (t - dt, e)) $
                           bsTimeouts botState

valueOfTag :: TagEntry -> T.Text
valueOfTag (TagEntry _ value) = value

handleIrcMessage :: Bot -> RawIrcMsg -> BotState -> IO BotState
handleIrcMessage b msg botState = do
  let badges = concat $
               maybeToList $
               fmap (T.splitOn "," . valueOfTag) $
               find (\(TagEntry ident _) -> ident == "badges") $
               _msgTags msg
  let cookedMsg = cookIrcMsg msg
  print cookedMsg
  case cookedMsg of
    (Ping xs) -> do
      atomically $ writeTQueue (bsOutcoming botState) (ircPong xs)
      return botState
    (Privmsg userInfo target msgText) ->
      applyEffectInTransaction botState $
      b $
      Msg Sender { senderName = idText $ userNick userInfo
                 , senderChannel = idText target
                 , senderSubscriber = any (T.isPrefixOf "subscriber") badges
                 , senderMod = any (T.isPrefixOf "moderator") badges
                 , senderBroadcaster = any (T.isPrefixOf "broadcaster") badges
                 } msgText
    _ -> return botState
