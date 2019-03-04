{-# LANGUAGE OverloadedStrings #-}
module Transport.Debug (debugTransportEntry) where

import Transport
import Config
import Control.Concurrent.STM
import qualified Data.Text as T
import System.IO
import Control.Monad

flushOutcomingMessages :: OutcomingQueue -> IO ()
flushOutcomingMessages outcoming = do
  msg <- atomically $ tryReadTQueue outcoming
  case msg of
    Just (OutMsg msg') -> do
      putStrLn $ T.unpack msg'
      flushOutcomingMessages outcoming
    Nothing -> return ()

debugRepl :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugRepl incoming outcoming config = do
  flushOutcomingMessages outcoming
  putStr "> "
  hFlush stdout
  message <- getLine
  when (not $ null message) $
    atomically $
    writeTQueue incoming $
    InMsg
      Sender
        { senderName = dbgOwner config
        , senderDisplayName = dbgOwner config
        -- TODO: Sender id, channel and roles are not configurable in Debug Mode
        --   We can make them configurable via the config file
        --   and/or via CLI commands.
        , senderChannel = TwitchChannel "#tsoding"
        , senderId = ""
        , senderSubscriber = True
        , senderMod = True
        , senderBroadcaster = True
        , senderOwner = True
        }
      (T.pack message)
  debugRepl incoming outcoming config

debugTransportEntry :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugTransportEntry incoming outcoming config = do
  atomically $ writeTQueue incoming $ Joined $ dbgNick config
  debugRepl incoming outcoming config
