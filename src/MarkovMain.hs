module Main where

import qualified Data.Text.IO as TIO
import Markov
import System.Environment

trainMain :: [String] -> IO ()
trainMain (input:output:_) = do
  markov <- file2Markov input
  saveMarkov output markov
trainMain _ = error "Usage: ./Markov train <input.txt> <output.csv>"

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
