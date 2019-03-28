{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , TwitchParams(..)
  , DiscordParams(..)
  , DebugParams(..)
  , configsFromFile
  ) where

import Data.Either.Extra
import qualified Data.HashMap.Strict as HM
import Data.Ini
import qualified Data.Text as T
import Discord
import Safe
import Text.InterpolatedString.QM

data Config
  = TwitchConfig TwitchParams
  | DiscordConfig DiscordParams
  | DebugConfig DebugParams
  deriving (Show)

data TwitchParams = TwitchParams
  { tpNick :: T.Text
  , tpPass :: T.Text
  , tpChannel :: T.Text
  , tpTwitchClientId :: T.Text
  , tpOwner :: T.Text
  } deriving (Show)

data DiscordParams = DiscordParams
  { dpAuthToken :: T.Text
  , dpChannel :: ChannelId
  , dpTwitchClientId :: T.Text
  , dpOwner :: T.Text
  } deriving (Show)

data DebugParams = DebugParams
  { dbgOwner :: T.Text
  , dbgTwitchClientId :: T.Text
  , dbgNick :: T.Text
  } deriving (Show)

twitchParamsFromIni :: T.Text -> Ini -> Either String TwitchParams
twitchParamsFromIni section ini =
  TwitchParams <$> lookupValue section "nick" ini <*>
  lookupValue section "password" ini <*>
  (T.cons '#' <$> lookupValue section "channel" ini) <*>
  lookupValue section "clientId" ini <*>
  lookupValue section "owner" ini

discordParamsFromIni :: T.Text -> Ini -> Either String DiscordParams
discordParamsFromIni section ini =
  DiscordParams <$> lookupValue section "authToken" ini <*>
  fmap
    Snowflake
    ((maybeToEither "channel is not a number" . readMay . T.unpack) =<<
     lookupValue section "channel" ini) <*>
  lookupValue section "clientId" ini <*>
  lookupValue section "owner" ini

debugParamsFromIni :: T.Text -> Ini -> Either String DebugParams
debugParamsFromIni section ini =
  DebugParams <$> lookupValue section "owner" ini <*>
  lookupValue section "clientId" ini <*>
  lookupValue section "nick" ini

configFromIniSection :: T.Text -> Ini -> Either String Config
configFromIniSection sectionName ini = do
  configType <- lookupValue sectionName "type" ini
  case configType of
    "twitch" -> TwitchConfig <$> twitchParamsFromIni sectionName ini
    "discord" -> DiscordConfig <$> discordParamsFromIni sectionName ini
    "debug" -> DebugConfig <$> debugParamsFromIni sectionName ini
    _ -> Left [qms|"Unrecognized config type: {configType}"|]

configsFromFile :: FilePath -> IO [Config]
configsFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $
    mapLeft ([qms|In file '{filePath}':\ |] <>) $ do
      bots <- filter (T.isPrefixOf "bot:") . HM.keys . unIni <$> ini
      ini >>= \ini' ->
        mapM
          (\section ->
             mapLeft ([qms|In section '{section}':\ |] <>) $
             configFromIniSection section ini')
          bots
