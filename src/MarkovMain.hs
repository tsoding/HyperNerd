{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Foldable
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Markov
import System.Environment
import Text.InterpolatedString.QM

newtype Text2Markov = Text2Markov
  { asMarkov :: Markov
  }

instance FromRow Text2Markov where
  fromRow = Text2Markov . text2Markov <$> field

instance Semigroup Text2Markov where
  m1 <> m2 = Text2Markov (asMarkov m1 <> asMarkov m2)

instance Monoid Text2Markov where
  mempty = Text2Markov mempty

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
      fold <$>
      SQLite.query_
        sqliteConn
        [qms|select ep1.propertyText
             from EntityProperty ep1
             where ep1.entityName = 'LogRecord'
               and ep1.propertyName = 'msg'|]
    saveMarkov output $ asMarkov markov
trainMain _ = error "Usage: ./Markov train <database.db> <output.csv>"

sayMain :: [String] -> IO ()
sayMain (input:_) = do
  markov <- loadMarkov input
  sentence <- eventsAsText <$> simulate markov
  TIO.putStrLn sentence
sayMain _ = error "Usage: ./Markov say <input.csv>"

mainWithArgs :: [String] -> IO ()
mainWithArgs ("train":args) = trainMain args
mainWithArgs ("say":args) = sayMain args
mainWithArgs _ = error "Usage: ./Markov <train|say>"

main :: IO ()
main = getArgs >>= mainWithArgs
