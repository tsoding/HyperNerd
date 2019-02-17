module Transport.Discord
  ( discordTransportEntry
  ) where

import Config
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, throwIO)
import Control.Monad (unless)
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
  , messageText
  , nextEvent
  , restCall
  , stopDiscord
  , userIsBot
  , userName
  )
import Transport

sendLoop :: ChannelId -> OutcomingQueue -> (RestChan, Gateway, z) -> IO ()
sendLoop channel outcoming dis = do
  outMsg <- atomically $ readTQueue outcoming
  case outMsg of
    OutMsg text -> do
      resp <- restCall dis (CreateMessage channel text)
      print resp
  sendLoop channel outcoming dis

fromBot :: D.Message -> Bool
fromBot = userIsBot . messageAuthor

receiveLoop ::
     T.Text -> ChannelId -> IncomingQueue -> (RestChan, Gateway, z) -> IO ()
receiveLoop owner channel incoming dis = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) ->
      unless (fromBot m) $ do
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

-- TODO(#457): Joined event is not send for Discord Transport
discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordParams -> IO ()
discordTransportEntry incoming outcoming conf = do
  bracket (loginRestGateway $ Auth $ dpAuthToken conf) stopDiscord $ \dis ->
    withAsync (sendLoop (dpChannel conf) outcoming dis) $ \sender ->
      withAsync (receiveLoop (dpOwner conf) (dpChannel conf) incoming dis) $ \receive -> do
        res <- waitEitherCatch sender receive
        case res of
          Left Right {} -> fail "PANIC: sendLoop returned"
          Right Right {} -> return ()
          Left (Left e) -> throwIO e
          Right (Left e) -> throwIO e
  return ()
