module Main where

import Bot
import Control.Exception
import Hookup
import Irc.RateLimit (RateLimit)
import System.Environment

data Config = Config { configNick :: String
                     , configPass :: String
                     , configRate :: RateLimit
                     }

conParamsFromConfig :: Config -> ConnectionParams
conParamsFromConfig config =
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

-- TODO: implement Main.configFromFile
configFromFile :: FilePath -> IO Config
configFromFile = undefined

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params body =
    bracket (connect params) close body

-- TODO: implement Main.ircTransport
ircTransport :: Bot s -> Config -> Connection -> IO ()
ircTransport = undefined

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath] =
    do config <- configFromFile configPath
       withConnection (conParamsFromConfig config) $ ircTransport bot config
mainWithArgs _ = error "./tsoder2 <config-file>"

main :: IO ()
main = getArgs >>= mainWithArgs
