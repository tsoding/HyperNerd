module Transport.Debug (debugTransportEntry) where

import Transport
import Config
import Control.Concurrent.STM

-- TODO: debugRepl is not implemented
debugRepl :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugRepl = undefined

debugTransportEntry :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugTransportEntry incoming outcoming config = do
  atomically $ writeTQueue incoming $ Joined $ dbgNick config
  debugRepl incoming outcoming config
