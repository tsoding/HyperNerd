{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Bot
import BotState
import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Clock
import System.Environment
import Text.InterpolatedString.QM
import Transport.Discord
import Transport.Twitch

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState = do
  threadDelay 10000 -- to prevent busy looping
  currCPUTime <- getTime Monotonic
  let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
  pollMessage <-
    maybe return (handleInEvent b) <$>
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

-- TODO: Bot spawns only single transport thread
--   It should use configsFromFile file to read several channel configuration from a single config file and spawn the transports accordingly
entry :: String -> String -> Maybe String -> IO ()
entry configPath databasePath markovPath =
  withBotState markovPath configPath databasePath $ \botState -> do
    supavisah $ logicEntry botState
    case bsConfig botState of
      TwitchConfig twitchConfig ->
        twitchTransportEntry
          (bsIncoming botState)
          (bsOutcoming botState)
          twitchConfig
      DiscordConfig discordConfig ->
        discordTransportEntry
          (bsIncoming botState)
          (bsOutcoming botState)
          discordConfig

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = entry configPath databasePath Nothing
mainWithArgs [configPath, databasePath, markovPath] =
  entry configPath databasePath (Just markovPath)
mainWithArgs _ = error "./HyperNerd <config-file> <database-file> [markov-file]"

main :: IO ()
main = getArgs >>= mainWithArgs
