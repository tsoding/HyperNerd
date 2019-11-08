{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

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
import System.IO
import Text.InterpolatedString.QM
import Transport

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

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

authorize :: TwitchConfig -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ tcPass conf)
  sendMsg conn (ircNick $ tcNick conf)
  sendMsg conn (ircJoin (tcChannel conf) Nothing)
  sendMsg conn (ircCapReq ["twitch.tv/tags"])

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine conn = do
  mb <-
    catch
      (recvLine conn maxIrcMessage)
      (\case
         LineTooLong -> do
           hPutStrLn stderr "[WARN] Received LineTooLong. Ignoring it..."
           return Nothing
         e -> throwIO e)
  for mb $ \xs ->
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! msg
      Nothing -> fail "Server sent invalid message!"

valueOfTag :: TagEntry -> T.Text
valueOfTag (TagEntry _ value) = value

receiveLoop :: TwitchConfig -> IncomingQueue -> Connection -> IO ()
receiveLoop conf incoming ircConn = do
  mb <- readIrcLine ircConn
  for_ mb $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    -- TODO(#483): Logs from different channels clash together
    --   Let's introduce logging to files. A file per channel.
    putStrLn [qms|[TWITCH] {cookedMsg}|]
    case cookedMsg of
      (Ping xs) -> sendMsg ircConn (ircPong xs)
      (Privmsg userInfo target msgText)
        | T.toLower (tcNick conf) /= T.toLower (idText $ userNick userInfo) ->
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
                    , TwitchVip <$ find (T.isPrefixOf "vip") badges
                    , TwitchBroadcaster <$
                      find (T.isPrefixOf "broadcaster") badges
                    , toMaybe TwitchBotOwner (name == tcOwner conf)
                    ]
            -- TODO(#468): Twitch does not provide the id of the user
              , senderId = fromMaybe "" userId
              }
            (T.toLower (tcNick conf) `T.isInfixOf` T.toLower msgText)
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
              userId =
                fmap (\(TagEntry _ value) -> value) $
                find (\(TagEntry ident _) -> ident == "user-id") $ _msgTags msg
      Join {} ->
        atomically $
        writeTQueue incoming $ Joined (TwitchChannel $ tcChannel conf)
      _ -> return ()
  receiveLoop conf incoming ircConn

sendLoop :: T.Text -> OutcomingQueue -> Connection -> IO ()
sendLoop channel outcoming ircConn = do
  outMsg <- atomically $ readTQueue outcoming
  -- TODO(#551): Twitch Transport does not split messages with newlines into several messages
  case outMsg of
    OutMsg _ text ->
      let twitchMessageLimit = 499
       in sendMsg ircConn $ ircPrivmsg channel $ T.take twitchMessageLimit text
  sendLoop channel outcoming ircConn

-- TODO(#17): check unsuccessful authorization
twitchTransportEntry :: Transport -> TwitchConfig -> IO ()
twitchTransportEntry transport conf = do
  withConnection twitchConnectionParams $ \ircConn -> do
    authorize conf ircConn
    withAsync (sendLoop (tcChannel conf) (trOutcoming transport) ircConn) $ \sender ->
      withAsync (receiveLoop conf (trIncoming transport) ircConn) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
-- TODO(#15): utilize rate limits
-- See https://github.com/glguy/irc-core/blob/6dd03dfed4affe6ae8cdd63ede68c88d70af9aac/bot/src/Main.hs#L32
