{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T
import Data.Ini
import Text.InterpolatedString.QM

data Config = TwitchConfig TwitchParams
            | DiscordConfig DiscordParams
              deriving (Show)

data TwitchParams = TwitchParams
  { tpNick :: T.Text
  , tpPass :: T.Text
  , tpChannel :: T.Text
  , tpClientId :: T.Text
  , tpOwner :: T.Text
  } deriving (Show)

data DiscordParams = DiscordParams
  { dpAuthToken :: T.Text
  , dpGuild :: T.Text
  , dpChannel :: T.Text
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
    lookupValue "Bot" "channel" ini

configFromFile :: FilePath -> IO Config
configFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $ do
    configType <- ini >>= lookupValue "Bot" "type"
    case configType of
      "twitch"  -> TwitchConfig <$> (ini >>= twitchParamsFromIni)
      "discord" -> DiscordConfig <$> (ini >>= discordParamsFromIni)
      _ -> Left [qms|"Unrecognized config type: {configType}"|]
