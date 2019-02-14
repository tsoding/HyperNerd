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
    TwitchConfig <$> (ini >>= lookupValue "User" "nick") <*>
    (ini >>= lookupValue "User" "password") <*>
    (T.cons '#' <$> (ini >>= lookupValue "User" "channel")) <*>
    (ini >>= lookupValue "User" "clientId") <*>
    (ini >>= lookupValue "User" "owner")
