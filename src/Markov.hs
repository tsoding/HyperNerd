{-# LANGUAGE OverloadedStrings #-}

module Markov where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import Safe
import System.Random

data Event
  = Begin
  | Word T.Text
  | End
  deriving (Show, Read, Eq, Ord)

eventWordPrefix :: BS.ByteString
eventWordPrefix = "word|"

instance FromField Event where
  parseField s
    | s == "begin" = pure Begin
    | s == "end" = pure End
    | eventWordPrefix `BS.isPrefixOf` s =
      pure $ Word $ decodeUtf8 $ BS.drop (BS.length eventWordPrefix) s
    | otherwise = mempty

instance ToField Event where
  toField Begin = "begin"
  toField End = "end"
  toField (Word word) = BS.append eventWordPrefix $ encodeUtf8 word

newtype Markov = Markov
  { asMap :: M.Map Event (M.Map Event Int)
  } deriving (Show, Read)

instance FromRecord Markov where
  parseRecord v
    | length v == 3 = record2Markov <$> v .! 0 <*> v .! 1 <*> v .! 2
    | otherwise = mempty

emptyMarkov :: Markov
emptyMarkov = Markov M.empty

combineMarkov :: Markov -> Markov -> Markov
combineMarkov m1 m2 =
  Markov $ M.unionWith (M.unionWith (+)) (asMap m1) (asMap m2)

instance Semigroup Markov where
  (<>) = combineMarkov

instance Monoid Markov where
  mempty = emptyMarkov

record2Markov :: Event -> Event -> Int -> Markov
record2Markov cause result occurance =
  Markov $ M.fromList [(cause, M.fromList [(result, occurance)])]

singleton :: (Event, Event) -> Markov
singleton (e1, e2) = record2Markov e1 e2 1

text2Markov :: T.Text -> Markov
text2Markov text =
  maybe mempty (foldMap singleton . zip events) (tailMay events)
  where
    events = [Begin] ++ map Word (T.words text) ++ [End]

log2Markov :: [T.Text] -> Markov
log2Markov = foldMap text2Markov . filter (not . T.isPrefixOf "!")

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
            zip (map fst statList) $ scanl (+) 0 $ map snd statList
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
  (event' :) <$> simulateFrom event' markov

simulate :: Markov -> IO [Event]
simulate = simulateFrom Begin

markov2Records :: Markov -> [(Event, Event, Int)]
markov2Records markov = do
  (cause, results) <- M.toList $ asMap markov
  (result, occurance) <- M.toList results
  return (cause, result, occurance)

saveMarkov :: FilePath -> Markov -> IO ()
saveMarkov filePath markov =
  BSL.writeFile filePath $ encode $ markov2Records markov

loadMarkov :: FilePath -> IO Markov
loadMarkov filePath = do
  input <- BSL.readFile filePath
  either error (return . fold) $ decode NoHeader input
