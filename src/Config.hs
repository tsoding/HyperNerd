{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , TwitchParams(..)
  , DiscordParams(..)
  , configFromFile
  , configsFromFile
  ) where

import Data.Either.Extra
import Data.Ini
import qualified Data.Text as T
import Discord
import Safe
import Text.InterpolatedString.QM
import qualified Data.HashMap.Strict as HM

data Config
  = TwitchConfig TwitchParams
  | DiscordConfig DiscordParams
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
  , dpGuild :: T.Text
  , dpChannel :: ChannelId
  , dpTwitchClientId :: T.Text
  , dpOwner :: T.Text
  } deriving (Show)

twitchParamsFromIni :: Ini -> Either String TwitchParams
twitchParamsFromIni ini =
  TwitchParams <$> lookupValue "Bot" "nick" ini <*>
  lookupValue "Bot" "password" ini <*>
  (T.cons '#' <$> lookupValue "Bot" "channel" ini) <*>
  lookupValue "Bot" "clientId" ini <*>
  lookupValue "Bot" "owner" ini

discordParamsFromIni :: Ini -> Either String DiscordParams
discordParamsFromIni ini =
  DiscordParams <$> lookupValue "Bot" "authToken" ini <*>
  lookupValue "Bot" "guild" ini <*>
  fmap
    Snowflake
    ((maybeToEither "channel is not a number" . readMay . T.unpack) =<<
     lookupValue "Bot" "channel" ini) <*>
  lookupValue "Bot" "clientId" ini <*>
  lookupValue "Bot" "owner" ini

configFromIniSection :: T.Text -> Ini -> Either String Config
configFromIniSection sectionName ini = do
  configType <- lookupValue sectionName "type" ini
  case configType of
    "twitch" -> TwitchConfig <$> (twitchParamsFromIni ini)
    "discord" -> DiscordConfig <$> (discordParamsFromIni ini)
    _ -> Left [qms|"Unrecognized config type: {configType}"|]

configFromFile :: FilePath -> IO Config
configFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return (ini >>= configFromIniSection "Bot")

configsFromFile :: FilePath -> IO [Config]
configsFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $ do
    bots <- HM.keys . unIni <$> ini
    ini >>= \ini' -> mapM (flip configFromIniSection ini') bots
