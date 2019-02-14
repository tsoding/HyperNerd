{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Bot
import BotState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Transport.Twitch
import System.Clock
import System.Environment
import Text.InterpolatedString.QM

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
  eventLoop bot currCPUTime botState

-- TODO(#399): supervisor is vulnerable to errors that happen at the start of the action
supavisah :: Show a => IO a -> IO ()
supavisah x =
  void $
  forkFinally x $ \reason -> do
    putStrLn [qms|Thread died because of {reason}. Restarting...|]
    supavisah x

entry :: String -> String -> Maybe String -> IO ()
entry configPath databasePath markovPath =
  withBotState markovPath configPath databasePath $ \botState -> do
    supavisah $ logicEntry botState
    twitchTransportEntry
      (bsIncoming botState)
      (bsOutcoming botState)
      (bsConfig botState)

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = entry configPath databasePath Nothing
mainWithArgs [configPath, databasePath, markovPath] =
  entry configPath databasePath (Just markovPath)
mainWithArgs _ = error "./HyperNerd <config-file> <database-file> [markov-file]"

main :: IO ()
main = getArgs >>= mainWithArgs
