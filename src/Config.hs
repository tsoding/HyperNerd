{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , TwitchConfig(..)
  , DiscordConfig(..)
  , GithubConfig(..)
  , configFromFile
  ) where

import Control.Monad
import Data.Either.Extra
import qualified Data.HashMap.Strict as HM
import Data.Ini
import qualified Data.Text as T
import Discord
import Safe
import Text.InterpolatedString.QM
import Control.Applicative

data Config = Config
  { configTwitch :: Maybe TwitchConfig
  , configDiscord :: Maybe DiscordConfig
  , configGithub :: Maybe GithubConfig
  }

newtype GithubConfig = GithubConfig
  { githubApiKey :: T.Text
  }

data TwitchConfig = TwitchConfig
  { tcNick :: T.Text
  , tcPass :: T.Text
  , tcChannel :: T.Text
  , tcTwitchClientId :: T.Text
  , tcOwner :: T.Text
  }

data DiscordConfig = DiscordConfig
  { dcAuthToken :: T.Text
  , dcChannels :: [ChannelId]
  , dcGuildId :: GuildId
  }

hmLookupValue :: T.Text -> HM.HashMap T.Text T.Text -> Either String T.Text
hmLookupValue field ini =
  maybeToEither [qms|Cannot find field {field}|] $ HM.lookup field ini

twitchConfigFromHm :: HM.HashMap T.Text T.Text -> Either String TwitchConfig
twitchConfigFromHm ini =
  TwitchConfig <$> hmLookupValue "nick" ini <*> hmLookupValue "password" ini <*>
  (T.cons '#' <$> hmLookupValue "channel" ini) <*>
  hmLookupValue "clientId" ini <*>
  hmLookupValue "owner" ini

discordConfigFromHm :: HM.HashMap T.Text T.Text -> Either String DiscordConfig
discordConfigFromHm ini =
  DiscordConfig <$> hmLookupValue "authToken" ini <*>
  fmap
    (map Snowflake)
    ((maybeToEither "`channels` is not a list of numbers" . readMay . T.unpack) =<<
     (hmLookupValue "channel" ini <|> hmLookupValue "channels" ini)) <*>
  fmap
    Snowflake
    ((maybeToEither "Guild ID is not a number" . readMay . T.unpack) =<<
     hmLookupValue "guildId" ini)

githubConfigFromHm :: HM.HashMap T.Text T.Text -> Either String GithubConfig
githubConfigFromHm ini = GithubConfig <$> hmLookupValue "apiKey" ini

configFromFile :: FilePath -> IO Config
configFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $
    mapLeft ([qms|In file '{filePath}':\ |] <>) $ do
      secs <- unIni <$> ini
      liftM3
        Config
        (sequence (twitchConfigFromHm <$> HM.lookup "twitch" secs))
        (sequence (discordConfigFromHm <$> HM.lookup "discord" secs))
        (sequence (githubConfigFromHm <$> HM.lookup "github" secs))
