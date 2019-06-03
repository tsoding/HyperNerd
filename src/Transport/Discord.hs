{-# LANGUAGE QuasiQuotes #-}

module Transport.Discord
  ( discordTransportEntry
  ) where

import Config
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, throwIO)
import Control.Monad (void, when)
import Data.List
import Data.Maybe
import Data.Maybe.Extra
import qualified Data.Text as T
import qualified Discord as D
import Discord
  ( Auth(..)
  , ChannelId
  , ChannelRequest(CreateMessage)
  , Event(..)
  , Gateway
  , RestChan
  , User(..)
  , loginRestGateway
  , messageAuthor
  , messageChannel
  , messageMentions
  , messageText
  , nextEvent
  , restCall
  , stopDiscord
  , userId
  , userIsBot
  , userName
  )
import Discord.Rest.User (UserRequest(GetCurrentUser))
import System.IO
import Text.InterpolatedString.QM
import Transport

sendLoop :: OutcomingQueue -> (RestChan, Gateway, z) -> IO ()
sendLoop outcoming dis = do
  outMsg <- atomically $ readTQueue outcoming
  case outMsg of
    OutMsg (DiscordChannel chanId) text ->
      void $ restCall dis (CreateMessage (D.Snowflake chanId) text)
    OutMsg channel text ->
      hPutStrLn
        stderr
        [qms|[ERROR] Tried to send {channel} message '{text}'
             from a Discord transport|]
  sendLoop outcoming dis

fromBot :: D.Message -> Bool
fromBot = userIsBot . messageAuthor

fromChannel :: ChannelId -> D.Message -> Bool
fromChannel channel message = messageChannel message == channel

rolesOfMessage :: (RestChan, Gateway, z) -> D.Message -> D.UserId -> IO [Role]
rolesOfMessage dis msg ownerId =
  case D.messageGuild msg of
    Just guildId -> do
      resp <-
        D.restCall dis $ D.GetGuildMember guildId (userId $ messageAuthor msg)
      case resp of
        Left e -> error $ show e
        Right guildMember ->
          let ownerRole =
                maybeToList $
                toMaybe
                  DiscordGuildOwner
                  (D.userId (D.messageAuthor msg) == ownerId)
           in return
                (map (DiscordRole . fromIntegral) (D.memberRoles guildMember) ++
                 ownerRole)
    Nothing -> do
      hPutStrLn
        stderr
        [qms|[WARNING] Could not extract a guild from the message {msg}|]
      return []

receiveLoop ::
     User
  -> D.UserId
  -> [ChannelId]
  -> IncomingQueue
  -> (RestChan, Gateway, z)
  -> IO ()
receiveLoop botUser ownerId channels incoming dis = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) ->
      when (not (fromBot m) && any (`fromChannel` m) channels) $ do
        print m
        let name = T.pack $ userName $ messageAuthor m
        -- TODO(#522): requesting Discord roles on each message is dangerous for rate limits
        roles <- rolesOfMessage dis m ownerId
        atomically $
          writeTQueue incoming $
          InMsg $
          Message
            Sender
              { senderName = name
              , senderDisplayName = name
              , senderChannel = DiscordChannel $ fromIntegral $ messageChannel m
              , senderId = T.pack $ show $ userId $ messageAuthor m
              , senderRoles = roles
              }
            (isJust $ find (== userId botUser) $ map userId $ messageMentions m)
            (messageText m)
    _ -> return ()
  receiveLoop botUser ownerId channels incoming dis

-- TODO(#465): Discord transport does not handle authorization failure
discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordConfig -> IO ()
discordTransportEntry incoming outcoming conf =
  bracket (loginRestGateway $ Auth $ dcAuthToken conf) stopDiscord $ \dis -> do
    respCurrentUser <- restCall dis GetCurrentUser
    -- TODO(#466): restCall errors are not handled properly
    -- TODO(#523): Discord transport never checks if the bot actually sitting in the dpChannels
    case respCurrentUser of
      Left _ -> error "Getting current user call failed"
      Right user -> do
        respGuild <- restCall dis $ D.GetGuild $ dcGuildId conf
        case respGuild of
          Left _ -> error "Getting guild id call failed"
          Right guild -> do
            mapM_
              (\chanId ->
                 atomically $
                 writeTQueue incoming $
                 Joined (DiscordChannel $ fromIntegral chanId)) $
              dcChannels conf
            withAsync (sendLoop outcoming dis) $ \sender ->
              withAsync
                (receiveLoop
                   user
                   (D.guildOwnerId guild)
                   (dcChannels conf)
                   incoming
                   dis) $ \receive -> do
                res <- waitEitherCatch sender receive
                case res of
                  Left Right {} -> fail "PANIC: sendLoop returned"
                  Right Right {} -> return ()
                  Left (Left e) -> throwIO e
                  Right (Left e) -> throwIO e
