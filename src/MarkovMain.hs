{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQLite
import Markov
import Safe
import System.Environment
import Text.InterpolatedString.QM

asteriskCorrectionFilter :: [T.Text] -> [T.Text]
asteriskCorrectionFilter = filter ((/= '*') . T.last)

mentionsFilter :: [T.Text] -> [T.Text]
mentionsFilter =
  filter (not . T.null) . map (T.unwords . filter ((/= '@') . T.head) . T.words)

commandsFilter :: [T.Text] -> [T.Text]
commandsFilter = filter ((/= '!') . T.head)

-- TODO(#430): Markov utility always build the model from scratch
--   1. Check if `output` file exists
--   2. Load the `output` file as Markov model `markov`
--   3. Check the modification date of the `output` file
--   4. Open the `databasePath` and fetch only the logs after the date
--   5. Top up the `markov` with the fresh data
trainMain :: [String] -> IO ()
trainMain (databasePath:output:_) =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    markov <-
      fold .
      map text2Markov .
      commandsFilter .
      mentionsFilter . asteriskCorrectionFilter . map SQLite.fromOnly <$>
      SQLite.query_
        sqliteConn
        [qms|select ep1.propertyText
             from EntityProperty ep1
             where ep1.entityName = 'LogRecord'
               and ep1.propertyName = 'msg'|]
    saveMarkov output markov
trainMain _ =
  error "Usage: ./Markov train <database:SqliteFile> <output:CsvFile>"

sayMain :: [String] -> IO ()
sayMain (input:strN:_) =
  case readMay strN of
    Just n -> do
      markov <- loadMarkov input
      replicateM_ n ((eventsAsText <$> simulate markov) >>= TIO.putStrLn)
    Nothing -> error "n is not a number"
sayMain (input:_) = do
  markov <- loadMarkov input
  sentence <- eventsAsText <$> simulate markov
  TIO.putStrLn sentence
sayMain _ = error "Usage: ./Markov say <input:CsvFile> [n:Int]"

mainWithArgs :: [String] -> IO ()
mainWithArgs ("train":args) = trainMain args
mainWithArgs ("say":args) = sayMain args
mainWithArgs _ = error "Usage: ./Markov <train|say>"

main :: IO ()
main = getArgs >>= mainWithArgs
