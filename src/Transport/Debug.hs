{-# LANGUAGE OverloadedStrings #-}

module Transport.Debug
  ( debugTransportEntry
  ) where

import Config
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import System.IO
import Transport

flushOutgoingMessages :: OutgoingQueue -> IO ()
flushOutgoingMessages outgoing = do
  msg <- atomically $ tryReadTQueue outgoing
  case msg of
    Just (OutMsg msg') -> do
      putStrLn $ T.unpack msg'
      flushOutgoingMessages outgoing
    Nothing -> return ()

debugRepl :: IncomingQueue -> OutgoingQueue -> DebugParams -> IO ()
debugRepl incoming outgoing config = do
  flushOutgoingMessages outgoing
  putStr "> "
  hFlush stdout
  message <- getLine
  unless (null message) $
    atomically $
    writeTQueue incoming $
    InMsg
      Sender
        { senderName = dbgOwner config
        , senderDisplayName = dbgOwner config
        -- TODO(#480): Sender id, channel and roles are not configurable in Debug Mode
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
  debugRepl incoming outgoing config

debugTransportEntry :: IncomingQueue -> OutgoingQueue -> DebugParams -> IO ()
debugTransportEntry incoming outgoing config = do
  atomically $ writeTQueue incoming $ Joined $ dbgNick config
  debugRepl incoming outgoing config
