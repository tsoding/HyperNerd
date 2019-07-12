{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Backdoor
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
import Transport
import Transport.Discord
import Transport.Twitch

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState = do
  threadDelay 10000 -- to prevent busy looping
  currCPUTime <- getTime Monotonic
  let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
  let transports = [bsTwitchTransport botState, bsDiscordTransport botState]
  messages <-
    fmap (join . map maybeToList) $
    atomically $ mapM (tryReadTQueue . trIncoming) transports
  foldrM (handleInEvent b) botState messages >>= advanceTimeouts deltaTime >>=
    eventLoop b currCPUTime

logicEntry :: BotState -> IO ()
logicEntry botState = do
  currCPUTime <- getTime Monotonic
  handleInEvent bot Started botState >>= eventLoop bot currCPUTime

-- TODO(#399): supervisor is vulnerable to errors that happen at the start of the action
supavisah :: Show a => IO a -> IO ()
supavisah x =
  void $
  forkFinally x $ \reason -> do
    putStrLn [qms|[WARN] Thread died because of {reason}. Restarting...|]
    supavisah x

block :: IO ()
block = do
  threadDelay 10000 -- to prevent busy looping
  block

entry :: String -> String -> Maybe String -> IO ()
entry configPath databasePath markovPath =
  withBotState markovPath configPath databasePath $ \botState -> do
    supavisah $ logicEntry botState
    case configTwitch $ bsConfig botState of
      Just twitchConfig ->
        supavisah $
        twitchTransportEntry (bsTwitchTransport botState) twitchConfig
      Nothing -> return ()
    case configDiscord $ bsConfig botState of
      Just discordConfig ->
        supavisah $
        discordTransportEntry (bsDiscordTransport botState) discordConfig
      Nothing -> return ()
    -- TODO(#678): backdoor port is not configurable
    networkEnv "3000" testConsole

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = entry configPath databasePath Nothing
mainWithArgs [configPath, databasePath, markovPath] =
  entry configPath databasePath (Just markovPath)
mainWithArgs _ = error "./HyperNerd <config-file> <database-file> [markov-file]"

main :: IO ()
main = getArgs >>= mainWithArgs
