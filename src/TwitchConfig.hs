{-# LANGUAGE OverloadedStrings #-}

module TwitchConfig where

import Data.Ini
import qualified Data.Text as T

data TwitchConfig = TwitchConfig
  { tcNick :: T.Text
  , tcPass :: T.Text
  , tcChannel :: T.Text
  , tcClientId :: T.Text
  , tcOwner :: T.Text
  } deriving (Show)

tcFromFile :: FilePath -> IO TwitchConfig
tcFromFile filePath = do
  ini <- readIniFile filePath
  either (ioError . userError) return $
    TwitchConfig <$> (ini >>= lookupValue "Twitch" "nick") <*>
    (ini >>= lookupValue "Twitch" "password") <*>
    (T.cons '#' <$> (ini >>= lookupValue "Twitch" "channel")) <*>
    (ini >>= lookupValue "Twitch" "clientId") <*>
    (ini >>= lookupValue "Twitch" "owner")
