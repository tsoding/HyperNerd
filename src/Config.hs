{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T
import           Data.Ini

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     , configChannel :: T.Text
                     , configClientId :: T.Text
                     } deriving Show

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       either (ioError . userError) return $
         Config <$> (ini >>= lookupValue "User" "nick")
                <*> (ini >>= lookupValue "User" "password")
                <*> (T.cons '#' <$> (ini >>= lookupValue "User" "channel"))
                <*> (ini >>= lookupValue "User" "clientId")
