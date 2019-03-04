module Transport.Debug (debugTransportEntry) where

import Transport
import Config
import Control.Concurrent.STM
import qualified Data.Text as T

-- TODO: flushOutcomingMessages is not implemented
flushOutcomingMessages :: OutcomingQueue -> IO ()
flushOutcomingMessages = undefined

debugRepl :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugRepl incoming outcoming config = do
  flushOutcomingMessages outcoming
  putStr "> "
  message <- getLine
  atomically $
    writeTQueue incoming $
    InMsg
      Sender
        { senderName = dbgOwner config
        , senderDisplayName = dbgOwner config
        , senderChannel = DebugChannel
        -- TODO: Sender id and roles are not configurable in Debug Mode
        --   We can make them configurable via the config file
        --   and/or via CLI commands.
        , senderId = ""
        , senderSubscriber = True
        , senderMod = True
        , senderBroadcaster = True
        , senderOwner = True
        }
      (T.pack message)

debugTransportEntry :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugTransportEntry incoming outcoming config = do
  atomically $ writeTQueue incoming $ Joined $ dbgNick config
  debugRepl incoming outcoming config
