{-# LANGUAGE OverloadedStrings #-}

module Markov where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random
import Data.List

data Event
  = Begin
  | Word T.Text
  | End
  deriving (Show, Read, Eq, Ord)

newtype Markov = Markov
  { asMap :: M.Map Event (M.Map Event Int)
  } deriving (Show, Read)

emptyMarkov :: Markov
emptyMarkov = Markov $ M.empty

combineMarkov :: Markov -> Markov -> Markov
combineMarkov m1 m2 =
  Markov $ M.unionWith (M.unionWith (+)) (asMap m1) (asMap m2)

instance Semigroup Markov where
  (<>) = combineMarkov

instance Monoid Markov where
  mempty = emptyMarkov

sentence :: [Event] -> [Event]
sentence events = [Begin] ++ events ++ [End]

singleton :: (Event, Event) -> Markov
singleton (e1, e2) = Markov $ M.fromList [(e1, M.fromList [(e2, 1)])]

text2Markov :: T.Text -> Markov
text2Markov text = foldMap singleton $ zip events $ tail events
    where events = sentence $ map Word $ T.words text

log2Markov :: [T.Text] -> Markov
log2Markov = foldMap text2Markov

file2Markov :: FilePath -> IO Markov
file2Markov filePath = log2Markov . T.lines <$> TIO.readFile filePath

nextEvent :: Markov -> Event -> IO Event
nextEvent markov event =
  case M.lookup event (asMap markov) of
    Just stat -> do
      let statList = M.toList stat
      let n = foldl' (+) 0 $ map snd statList
      i <- randomRIO (0, n - 1)
      let a =
            dropWhile (\x -> snd x < i) $
            zip (map fst statList) $
            scanl (+) 0 $
            map snd statList
      case a of
        [] -> return End
        (event', _):_ -> return event'
    Nothing -> return End

eventsAsText :: [Event] -> T.Text
eventsAsText [] = ""
eventsAsText (Begin:rest) = eventsAsText rest
eventsAsText (End:rest) = eventsAsText rest
eventsAsText (Word word:rest) = T.concat [word, " ", eventsAsText rest]

simulateFrom :: Event -> Markov -> IO [Event]
simulateFrom End _ = return []
simulateFrom event markov = do
  event' <- nextEvent markov event
  (event':) <$> simulateFrom event' markov

simulate :: Markov -> IO [Event]
simulate = simulateFrom Begin
