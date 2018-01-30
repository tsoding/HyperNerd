module Main where

import Bot
import Control.Exception
import Control.Monad
import Data.Ini
import Hookup
import Irc.RateLimit (RateLimit)
import System.Environment
import qualified Data.Text as T

data Config = Config { configNick :: T.Text
                     , configPass :: T.Text
                     } deriving Show

config :: T.Text -> T.Text -> Config
config nick password = Config { configNick = nick
                              , configPass = password
                              }

configFromFile :: FilePath -> IO Config
configFromFile filePath =
    do ini <- readIniFile filePath
       let lookup section key = ini >>= lookupValue (T.pack section) (T.pack key)
       let nick = lookup "User" "nick"
       let password = lookup "User" "password"
       either (ioError . userError) return $ liftM2 config nick password

twitchConnectionParams :: ConnectionParams
twitchConnectionParams =
    ConnectionParams { cpHost = "irc.chat.twitch.tv"
                     , cpPort = 443
                     , cpTls = Just TlsParams { tpClientCertificate = Nothing
                                              , tpClientPrivateKey = Nothing
                                              , tpServerCertificate = Nothing
                                              , tpCipherSuite = "HIGH"
                                              , tpInsecure = False
                                              }
                     , cpSocks = Nothing
                     }

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params body =
    bracket (connect params) close body

-- TODO(#8): implement Main.ircTransport
ircTransport :: Bot s -> Config -> Connection -> IO ()
ircTransport = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath] =
    do config <- configFromFile configPath
       withConnection twitchConnectionParams $ ircTransport bot config
mainWithArgs _ = error "./HyperNerd <config-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
