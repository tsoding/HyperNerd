module Transport.Discord
  ( discordTransportEntry
  ) where

import Config
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, throwIO)
import Control.Monad (void, when)
import qualified Data.Text as T
import qualified Discord as D
import Discord
  ( Auth(..)
  , ChannelId
  , ChannelRequest(CreateMessage)
  , Event(..)
  , Gateway
  , RestChan
  , loginRestGateway
  , messageAuthor
  , messageChannel
  , messageText
  , nextEvent
  , restCall
  , stopDiscord
  , userIsBot
  , userName
  )
import Discord.Rest.User (UserRequest(GetCurrentUser))
import Transport

sendLoop :: ChannelId -> OutcomingQueue -> (RestChan, Gateway, z) -> IO ()
sendLoop channel outcoming dis = do
  outMsg <- atomically $ readTQueue outcoming
  case outMsg of
    OutMsg text -> void $ restCall dis (CreateMessage channel text)
  sendLoop channel outcoming dis

fromBot :: D.Message -> Bool
fromBot = userIsBot . messageAuthor

fromChannel :: ChannelId -> D.Message -> Bool
fromChannel channel message = messageChannel message == channel

receiveLoop ::
     T.Text -> ChannelId -> IncomingQueue -> (RestChan, Gateway, z) -> IO ()
receiveLoop owner channel incoming dis = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) ->
      when (not (fromBot m) && fromChannel channel m) $ do
        let name = T.pack $ userName $ messageAuthor m
        atomically $
          writeTQueue incoming $
          InMsg
            Sender
              { senderName = name
              , senderDisplayName = name
              , senderChannel = T.pack $ show channel
              -- TODO(#455): Subscribers are not detected by Discord transport
              , senderSubscriber = False
              -- TODO(#456): Mods are not detected by Discord transport
              , senderMod = False
              , senderBroadcaster = False
              , senderOwner = name == owner
              }
            (messageText m)
    _ -> return ()
  receiveLoop owner channel incoming dis

-- TODO(#465): Discord transport does not handle authorization failure
discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordParams -> IO ()
discordTransportEntry incoming outcoming conf = do
  bracket (loginRestGateway $ Auth $ dpAuthToken conf) stopDiscord $ \dis -> do
    resp <- restCall dis GetCurrentUser
    -- TODO(#466): restCall errors are not handled properly
    case resp of
      Left _ -> error "Getting current user call failed"
      Right user ->
        atomically $ writeTQueue incoming $ Joined $ T.pack $ userName user
    withAsync (sendLoop (dpChannel conf) outcoming dis) $ \sender ->
      withAsync (receiveLoop (dpOwner conf) (dpChannel conf) incoming dis) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
