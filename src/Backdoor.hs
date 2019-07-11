{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Backdoor
  ( testConsole
  , consoleEnv
  , evalStateT
  , HandleChannel(..)
  , stdioChannel
  ) where

import Command
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine, hPutStr)
import Control.Monad.Trans.State.Strict
import System.IO

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
