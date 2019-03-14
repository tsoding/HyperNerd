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

debugChannel :: Channel
debugChannel = TwitchChannel "#tsoding"

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
  unless (null message) $
    atomically $
    writeTQueue incoming $
    InMsg $
    Message
      Sender
        { senderName = dbgOwner config
        , senderDisplayName = dbgOwner config
        -- TODO(#480): Sender id, channel and roles are not configurable in Debug Mode
        --   We can make them configurable via the config file
        --   and/or via CLI commands.
        , senderChannel = debugChannel
        , senderId = ""
        , senderSubscriber = True
        , senderMod = True
        , senderBroadcaster = True
        , senderOwner = True
        }
      (T.toLower (dbgNick config) `T.isInfixOf` T.toLower (T.pack message))
      (T.pack message)
  debugRepl incoming outcoming config

debugTransportEntry :: IncomingQueue -> OutcomingQueue -> DebugParams -> IO ()
debugTransportEntry incoming outcoming config = do
  atomically $ writeTQueue incoming $ Joined debugChannel $ dbgNick config
  debugRepl incoming outcoming config
