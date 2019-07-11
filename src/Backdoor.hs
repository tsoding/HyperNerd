{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Backdoor
  ( testConsole
  , consoleEnv
  , evalStateT
  , HandleChannel(..)
  , stdioChannel
  , networkEnv
  ) where

import Command
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine, hPutStr)
import Network.Socket
import System.IO

-- TODO(#675): Console does not have any way to exit
--   You can always close the connection, but in telnet for instance
--   you have to press an extra key stroke.
-- TODO(#676): Is ReaderT applicable for Backdoor (instead of StateT)
newtype Console s = Console
  { runCommand :: Command [T.Text] -> StateT s IO ()
  }

testConsole :: Channel s => Console s
testConsole =
  Console $ \command ->
    StateT $ \channel -> do
      case commandName command of
        "hello" ->
          case commandArgs command of
            (name:_) -> putText channel ("Hello, " <> name <> "\n")
            [] -> putText channel "Name is expected for hello command\n"
        "crash" -> error "Just crash 4HEad"
        unknown -> putText channel ("Unknown command: " <> unknown <> "\n")
      return ((), channel)

parseCommand :: T.Text -> Maybe (Command [T.Text])
parseCommand (T.words -> (name:args)) = return $ Command name args
parseCommand _ = Nothing

data HandleChannel = HandleChannel
  { handleInput :: Handle
  , handleOutput :: Handle
  }

stdioChannel :: HandleChannel
stdioChannel = HandleChannel stdin stdout

class Channel s where
  getTextLine :: s -> IO T.Text
  putText :: s -> T.Text -> IO ()

instance Channel HandleChannel where
  getTextLine channel = T.hGetLine $ handleInput channel
  putText channel s = do
    T.hPutStr (handleOutput channel) s
    hFlush (handleOutput channel)

promptTextLine :: Channel s => T.Text -> StateT s IO T.Text
promptTextLine prompt =
  StateT $ \channel -> do
    putText channel prompt
    line <- getTextLine channel
    return (line, channel)

consoleEnv :: Channel s => Console s -> StateT s IO ()
consoleEnv console = do
  line <- promptTextLine "> "
  maybe (return ()) (runCommand console) $ parseCommand line
  consoleEnv console

-- TODO: should networkEnv return StateT instead of IO
networkEnv :: String -> Console HandleChannel -> IO ()
networkEnv port' console = do
  addr <- resolve port'
  bracket (open addr) close loop
  where
    resolve port = do
      let hints =
            defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      setCloseOnExecIfNeeded $ fdSocket sock
      listen sock 10
      return sock
    loop sock =
      forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (const $ close conn)
    talk conn = do
      connHandle <- socketToHandle conn ReadWriteMode
      evalStateT (consoleEnv console) $ HandleChannel connHandle connHandle
