{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (finally)
import Control.Monad (when)
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Data.Char (toLower)
import Markov

loopingPing :: (RestChan, Gateway, z) -> Markov -> IO ()
loopingPing dis markov = do
  e <- nextEvent dis
  case e of
    Left er -> putStrLn ("Event error: " <> show er)
    Right (MessageCreate m) -> do
      when (isPing (messageText m) && not (fromBot m)) $ do
        markovText <- eventsAsText <$> simulate markov
        resp <- restCall dis (CreateMessage (messageChannel m) markovText)
        putStrLn (show resp)
        putStrLn ""
      loopingPing dis markov
    _ -> loopingPing dis markov

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

mainWithArgs :: [String] -> IO ()
mainWithArgs (authTokenFile:markovCsvFile:_) = do
  tok <- T.strip <$> TIO.readFile authTokenFile
  dis <- loginRestGateway (Auth tok)
  markov <- loadMarkov markovCsvFile
  finally (loopingPing dis markov)
          (stopDiscord dis)
mainWithArgs _ = error "Usage: Discord <auth-token-file> <markov.csv>"

-- TODO(#448): How can we integrate Discord Bot proof-of-concept with HyperNerd?
main :: IO ()
main = getArgs >>= mainWithArgs
