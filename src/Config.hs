{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , TwitchParams(..)
  , DiscordParams(..)
  , configFromFile
  ) where

import Data.Ini
import qualified Data.Text as T
import Text.InterpolatedString.QM
import Discord
import Safe
import Data.Either.Extra

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

configFromFile :: FilePath -> IO Config
configFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $ do
    configType <- ini >>= lookupValue "Bot" "type"
    case configType of
      "twitch" -> TwitchConfig <$> (ini >>= twitchParamsFromIni)
      "discord" -> DiscordConfig <$> (ini >>= discordParamsFromIni)
      _ -> Left [qms|"Unrecognized config type: {configType}"|]
