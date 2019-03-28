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

receiveLoop ::
     User
  -> T.Text
  -> [ChannelId]
  -> IncomingQueue
  -> (RestChan, Gateway, z)
  -> IO ()
receiveLoop botUser owner channels incoming dis = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) ->
      when (not (fromBot m) && any (`fromChannel` m) channels) $ do
        print m
        let name = T.pack $ userName $ messageAuthor m
        atomically $
          writeTQueue incoming $
          InMsg $
          Message
            Sender
              { senderName = name
              , senderDisplayName = name
              , senderChannel = DiscordChannel $ fromIntegral $ messageChannel m
              , senderId = T.pack $ show $ userId $ messageAuthor m
              -- TODO(#455): Subscribers are not detected by Discord transport
              , senderSubscriber = False
              -- TODO(#456): Mods are not detected by Discord transport
              , senderMod = False
              , senderBroadcaster = False
              , senderOwner = name == owner
              }
            (isJust $ find (== userId botUser) $ map userId $ messageMentions m)
            (messageText m)
    _ -> return ()
  receiveLoop botUser owner channels incoming dis

-- TODO(#465): Discord transport does not handle authorization failure
discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordParams -> IO ()
discordTransportEntry incoming outcoming conf = do
  bracket (loginRestGateway $ Auth $ dpAuthToken conf) stopDiscord $ \dis -> do
    resp <- restCall dis GetCurrentUser
    -- TODO(#466): restCall errors are not handled properly
    -- TODO: Discord transport never checks if the bot actually sitting in the dpChannels
    case resp of
      Left _ -> error "Getting current user call failed"
      Right user -> do
        mapM_
          (\chanId ->
             atomically $
             writeTQueue incoming $
             Joined
               (DiscordChannel $ fromIntegral chanId)
               (T.pack $ userName user)) $
          dpChannels conf
        withAsync (sendLoop outcoming dis) $ \sender ->
          withAsync
            (receiveLoop user (dpOwner conf) (dpChannels conf) incoming dis) $ \receive -> do
            res <- waitEitherCatch sender receive
            case res of
              Left Right {} -> fail "PANIC: sendLoop returned"
              Right Right {} -> return ()
              Left (Left e) -> throwIO e
              Right (Left e) -> throwIO e
  return ()
