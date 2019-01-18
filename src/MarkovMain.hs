module Main where

import System.Environment
import Markov
import qualified Data.Text.IO as TIO

mainWithArgs :: [String] -> IO ()
mainWithArgs ("train":input:output:_) = do
  markov <- file2Markov input
  saveMarkov output markov
mainWithArgs ("say":input:_) = do
  markov <- loadMarkov input
  sentence <- eventsAsText <$> simulate markov
  TIO.putStrLn sentence
mainWithArgs _ = error "Usage: ./Markov <train|say>"

main :: IO ()
main = getArgs >>= mainWithArgs
