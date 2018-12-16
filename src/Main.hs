{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot
import BotState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import IrcTransport
import System.Clock
import System.Environment

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState = do
  threadDelay 10000 -- to prevent busy looping
  currCPUTime <- getTime Monotonic
  let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
  pollMessage <-
    maybe return (handleIrcMessage b) <$>
    atomically (tryReadTQueue $ bsIncoming botState)
  pollMessage botState >>= advanceTimeouts deltaTime >>= eventLoop b currCPUTime

logicEntry :: BotState -> IO ()
logicEntry botState = do
  currCPUTime <- getTime Monotonic
  joinChannel bot botState >>= eventLoop bot currCPUTime

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = do
  withBotState configPath databasePath $ \botState -> do
    void $
      forkIO $
      ircTransportEntry
        (bsIncoming botState)
        (bsOutcoming botState)
        (bsConfig botState)
    logicEntry botState
mainWithArgs _ = error "./HyperNerd <config-file> <database-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
