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

-- TODO(#527): Discord transport doesn't set up DiscordGuildOwner role properly
rolesOfMessage :: (RestChan, Gateway, z) -> D.Message -> IO [Role]
rolesOfMessage dis msg =
  case D.messageGuild msg of
    Just guildId -> do
      resp <-
        D.restCall dis $ D.GetGuildMember guildId (userId $ messageAuthor msg)
      case resp of
        Left e -> error $ show e
        Right guildMember ->
          return $ map (DiscordRole . fromIntegral) $ D.memberRoles guildMember
    Nothing -> do
      hPutStrLn
        stderr
        [qms|[WARNING] Could not extract a guild from the message {msg}|]
      return []

receiveLoop ::
     User -> [ChannelId] -> IncomingQueue -> (RestChan, Gateway, z) -> IO ()
receiveLoop botUser channels incoming dis = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) ->
      when (not (fromBot m) && any (`fromChannel` m) channels) $ do
        print m
        let name = T.pack $ userName $ messageAuthor m
        -- TODO(#522): requesting Discord roles on each message is dangerous for rate limits
        roles <- rolesOfMessage dis m
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
  receiveLoop botUser channels incoming dis

-- TODO(#465): Discord transport does not handle authorization failure
discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordParams -> IO ()
discordTransportEntry incoming outcoming conf = do
  bracket (loginRestGateway $ Auth $ dpAuthToken conf) stopDiscord $ \dis -> do
    resp <- restCall dis GetCurrentUser
    -- TODO(#466): restCall errors are not handled properly
    -- TODO(#523): Discord transport never checks if the bot actually sitting in the dpChannels
    case resp of
      Left _ -> error "Getting current user call failed"
      Right user -> do
        mapM_
          (\chanId ->
             atomically $
             writeTQueue incoming $
             Joined
               (DiscordChannel $ fromIntegral chanId)) $
          dpChannels conf
        withAsync (sendLoop outcoming dis) $ \sender ->
          withAsync (receiveLoop user (dpChannels conf) incoming dis) $ \receive -> do
            res <- waitEitherCatch sender receive
            case res of
              Left Right {} -> fail "PANIC: sendLoop returned"
              Right Right {} -> return ()
              Left (Left e) -> throwIO e
              Right (Left e) -> throwIO e
  return ()
