{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Bot
import BotState
import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Maybe
import System.Clock
import System.Environment
import Text.InterpolatedString.QM
import Transport.Debug
import Transport.Discord
import Transport.Twitch

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState = do
  threadDelay 10000 -- to prevent busy looping
  currCPUTime <- getTime Monotonic
  let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
  messages <-
    fmap (join . map maybeToList) $
    atomically $ mapM (tryReadTQueue . csIncoming) $ bsChannels botState
  foldrM (handleInEvent b) botState messages >>= advanceTimeouts deltaTime >>=
    eventLoop b currCPUTime

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

block :: IO ()
block = do
  threadDelay 10000 -- to prevent busy looping
  block

-- TODO(#476): Bot spawns only single transport thread
--   It should use configsFromFile file to read several channel configuration from a single config file and spawn the transports accordingly
entry :: String -> String -> Maybe String -> IO ()
entry configPath databasePath markovPath =
  withBotState markovPath configPath databasePath $ \botState -> do
    supavisah $ logicEntry botState
    mapM_
      (\channelState ->
         case csConfig channelState of
           TwitchConfig twitchConfig ->
             void $
             forkIO $
             twitchTransportEntry
               (csIncoming channelState)
               (csOutcoming channelState)
               twitchConfig
           DiscordConfig discordConfig ->
             void $
             forkIO $
             discordTransportEntry
               (csIncoming channelState)
               (csOutcoming channelState)
               discordConfig
           DebugConfig debugConfig ->
             void $
             forkIO $
             debugTransportEntry
               (csIncoming channelState)
               (csOutcoming channelState)
               debugConfig) $
      bsChannels botState
    block

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = entry configPath databasePath Nothing
mainWithArgs [configPath, databasePath, markovPath] =
  entry configPath databasePath (Just markovPath)
mainWithArgs _ = error "./HyperNerd <config-file> <database-file> [markov-file]"

main :: IO ()
main = getArgs >>= mainWithArgs
