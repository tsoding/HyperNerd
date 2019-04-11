{-# LANGUAGE OverloadedStrings #-}

module Transport.Twitch
  ( twitchTransportEntry
  ) where

import Config
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Maybe.Extra
import qualified Data.Text as T
import Data.Traversable
import Hookup
import Irc.Commands (ircCapReq, ircJoin, ircNick, ircPass, ircPong, ircPrivmsg)
import Irc.Identifier (idText)
import Irc.Message (IrcMsg(Join, Ping, Privmsg), cookIrcMsg)
import Irc.RawIrcMsg
  ( RawIrcMsg(..)
  , TagEntry(..)
  , asUtf8
  , parseRawIrcMsg
  , renderRawIrcMsg
  )
import Irc.UserInfo (userNick)
import Network.Socket (Family(..))
import Transport

maxIrcMessage :: Int
maxIrcMessage = 1000

twitchConnectionParams :: ConnectionParams
twitchConnectionParams =
  ConnectionParams
    { cpHost = "irc.chat.twitch.tv"
    , cpPort = 443
    , cpTls =
        Just
          TlsParams
            { tpClientCertificate = Nothing
            , tpClientPrivateKey = Nothing
            , tpServerCertificate = Nothing
            , tpCipherSuite = "HIGH"
            , tpInsecure = False
            }
    , cpSocks = Nothing
    , cpFamily = AF_INET
    }

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

authorize :: TwitchParams -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ tpPass conf)
  sendMsg conn (ircNick $ tpNick conf)
  sendMsg conn (ircJoin (tpChannel conf) Nothing)
  sendMsg conn (ircCapReq ["twitch.tv/tags"])

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine conn = do
  mb <- recvLine conn maxIrcMessage
  for mb $ \xs ->
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! msg
      Nothing -> fail "Server sent invalid message!"

valueOfTag :: TagEntry -> T.Text
valueOfTag (TagEntry _ value) = value

receiveLoop :: TwitchParams -> IncomingQueue -> Connection -> IO ()
receiveLoop conf incoming ircConn = do
  mb <- readIrcLine ircConn
  for_ mb $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    -- TODO(#483): Logs from different channels clash together
    --   Let's introduce logging to files. A file per channel.
    print cookedMsg
    case cookedMsg of
      (Ping xs) -> sendMsg ircConn (ircPong xs)
      (Privmsg userInfo target msgText) ->
        atomically $
        writeTQueue incoming $
        InMsg $
        Message
          Sender
            { senderName = name
            , senderDisplayName = displayName
            , senderChannel = TwitchChannel $ idText target
            , senderRoles =
                catMaybes
                  [ TwitchSub <$ find (T.isPrefixOf "subscriber") badges
                  , TwitchMod <$ find (T.isPrefixOf "moderator") badges
                  , TwitchBroadcaster <$
                    find (T.isPrefixOf "broadcaster") badges
                  , toMaybe TwitchBotOwner (name == tpOwner conf)
                  ]
            -- TODO(#468): Twitch does not provide the id of the user
            , senderId = ""
            }
          (T.toLower (tpNick conf) `T.isInfixOf` T.toLower msgText)
          msgText
        where name = idText $ userNick userInfo
              displayName =
                maybe name valueOfTag $
                find (\(TagEntry ident _) -> ident == "display-name") $
                _msgTags msg
              badges =
                concat $
                maybeToList $
                fmap (T.splitOn "," . valueOfTag) $
                find (\(TagEntry ident _) -> ident == "badges") $ _msgTags msg
      Join {} ->
        atomically $
        writeTQueue incoming $ Joined (TwitchChannel $ tpChannel conf)
      _ -> return ()
  receiveLoop conf incoming ircConn

sendLoop :: T.Text -> OutcomingQueue -> Connection -> IO ()
sendLoop channel outcoming ircConn = do
  outMsg <- atomically $ readTQueue outcoming
  -- TODO(#551): Twitch Transport does not split messages with newlines into several messages
  case outMsg of
    OutMsg _ text -> sendMsg ircConn $ ircPrivmsg channel text
  sendLoop channel outcoming ircConn

-- TODO(#17): check unsuccessful authorization
twitchTransportEntry :: IncomingQueue -> OutcomingQueue -> TwitchParams -> IO ()
twitchTransportEntry incoming outcoming conf = do
  withConnection twitchConnectionParams $ \ircConn -> do
    authorize conf ircConn
    withAsync (sendLoop (tpChannel conf) outcoming ircConn) $ \sender ->
      withAsync (receiveLoop conf incoming ircConn) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32
