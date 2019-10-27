{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Expr
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Text as T
import HyperNerd.Parser
import System.Environment
import System.Random
import Text.Printf

data FuzzStat = FuzzStat
  { fsTextCount :: Int
  , fsMaxTextLen :: Int
  , fsMinTextLen :: Int
  , fsVarCount :: Int
  , fsFunCount :: Int
  , fsMaxFunArgsCount :: Int
  , fsMinFunArgsCount :: Int
  } deriving (Show, Eq)

instance Semigroup FuzzStat where
  s1 <> s2 =
    FuzzStat
      { fsTextCount = fsTextCount s1 + fsTextCount s2
      , fsMaxTextLen = fsMaxTextLen s1 `max` fsMaxTextLen s2
      , fsMinTextLen = fsMinTextLen s1 `min` fsMinTextLen s2
      , fsVarCount = fsVarCount s1 + fsVarCount s2
      , fsFunCount = fsFunCount s1 + fsFunCount s2
      , fsMaxFunArgsCount = fsMaxFunArgsCount s1 `max` fsMaxFunArgsCount s2
      , fsMinFunArgsCount = fsMinFunArgsCount s1 `min` fsMinFunArgsCount s2
      }

instance Monoid FuzzStat where
  mempty =
    FuzzStat
      { fsTextCount = 0
      , fsMaxTextLen = minBound
      , fsMinTextLen = maxBound
      , fsVarCount = 0
      , fsFunCount = 0
      , fsMaxFunArgsCount = minBound
      , fsMinFunArgsCount = maxBound
      }

statOfExprs :: [Expr] -> FuzzStat
statOfExprs = foldMap statOfExpr

statOfExpr :: Expr -> FuzzStat
statOfExpr (TextExpr text) =
  mempty
    { fsTextCount = 1
    , fsMinTextLen = T.length text
    , fsMaxTextLen = T.length text
    }
statOfExpr (VarExpr _) = mempty {fsVarCount = 1}
statOfExpr (FunCallExpr _ args) =
  mempty
    { fsFunCount = 1
    , fsMaxFunArgsCount = length args
    , fsMinFunArgsCount = length args
    } <>
  statOfExprs args

data FuzzParams = FuzzParams
  { fpFuzzCount :: Int
  , fpExprsRange :: (Int, Int)
  , fpFunCallArgsRange :: (Int, Int)
  , fpWordLenRange :: (Int, Int)
  , fpTextWordCountRange :: (Int, Int)
  } deriving (Show, Eq)

instance ToJSON FuzzParams where
  toJSON params =
    object
      [ "FuzzCount" .= fpFuzzCount params
      , "ExprsRange" .= fpExprsRange params
      , "FunCallArgsRange" .= fpFunCallArgsRange params
      , "WordLenRange" .= fpWordLenRange params
      , "TextWordCountRange" .= fpTextWordCountRange params
      ]

instance FromJSON FuzzParams where
  parseJSON (Object params) =
    FuzzParams <$> params .: "FuzzCount" <*> params .: "ExprsRange" <*>
    params .: "FunCallArgsRange" <*>
    params .: "WordLenRange" <*>
    params .: "TextWordCountRange"
  parseJSON invalid = typeMismatch "FuzzParams" invalid

readFuzzParams :: FilePath -> IO FuzzParams
readFuzzParams = fmap (either error id) . eitherDecodeFileStrict

saveFuzzParams :: FuzzParams -> FilePath -> IO ()
saveFuzzParams params filePath = BS.writeFile filePath $ encode params

defaultFuzzParams :: FuzzParams
defaultFuzzParams =
  FuzzParams
    { fpFuzzCount = 100
    , fpExprsRange = (1, 100)
    , fpFunCallArgsRange = (0, 2)
    , fpWordLenRange = (2, 10)
    , fpTextWordCountRange = (3, 5)
    }

unparseFunCallArg :: Expr -> T.Text
unparseFunCallArg (TextExpr text) = "\"" <> text <> "\""
unparseFunCallArg e = unparseExpr e

unparseFunCallArgs :: [Expr] -> T.Text
unparseFunCallArgs = T.concat . intersperse "," . map unparseFunCallArg

unparseExpr :: Expr -> T.Text
unparseExpr (TextExpr text) = text
unparseExpr (VarExpr name) = "%" <> name <> "%"
unparseExpr (FunCallExpr name args) =
  "%" <> name <> "(" <> unparseFunCallArgs args <> ")"

unparseExprs :: [Expr] -> T.Text
unparseExprs = T.concat . map unparseExpr

randomChar :: IO Char
randomChar = do
  x <- randomRIO (0, ord 'z' - ord 'a')
  return $ chr (x + ord 'a')

randomText :: FuzzParams -> IO T.Text
randomText params = do
  n <- randomRIO $ fpTextWordCountRange params
  T.concat . intersperse " " <$> replicateM n (randomWord params)

randomWord :: FuzzParams -> IO T.Text
randomWord params = do
  n <- randomRIO $ fpWordLenRange params
  T.pack <$> replicateM n randomChar

randomTextExpr :: FuzzParams -> IO Expr
randomTextExpr params = TextExpr <$> randomText params

randomVarExpr :: FuzzParams -> IO Expr
randomVarExpr params = VarExpr <$> randomWord params

randomFunCallExpr :: FuzzParams -> IO Expr
randomFunCallExpr params = do
  name <- randomWord params
  n <- randomRIO $ fpFunCallArgsRange params
  args <- replicateM n (randomExpr params)
  return $ FunCallExpr name args

randomExpr :: FuzzParams -> IO Expr
randomExpr params = do
  n <- randomRIO (0, 2) :: IO Int
  case n of
    0 -> randomTextExpr params
    1 -> randomVarExpr params
    _ -> randomFunCallExpr params

randomExprs :: FuzzParams -> IO [Expr]
randomExprs params = randomRIO (fpExprsRange params) >>= f []
  where
    normalizeExprs :: [Expr] -> [Expr]
    normalizeExprs [] = []
    normalizeExprs (TextExpr t1:TextExpr t2:rest) =
      normalizeExprs (TextExpr (t1 <> t2) : rest)
    normalizeExprs (x:rest) = x : normalizeExprs rest
    f :: [Expr] -> Int -> IO [Expr]
    f es n
      | m >= n = return es
      | otherwise = do
        es' <- replicateM (n - m) (randomExpr params)
        f (normalizeExprs (es ++ es')) n
      where
        m = length es

fuzzIteration :: FuzzParams -> IO FuzzStat
fuzzIteration params = do
  es <- randomExprs params
  let es' = runParser exprs $ unparseExprs es
  when (Right ("", es) /= es') $ do
    print es
    print es'
    error "Failed"
  return $ statOfExprs es

fuzz :: FuzzParams -> IO ()
fuzz params = do
  stats <- replicateM (fpFuzzCount params) (fuzzIteration params)
  print $ fold stats

mainWithArgs :: [String] -> IO ()
mainWithArgs ("genconf":configFilePath:_) = do
  saveFuzzParams defaultFuzzParams configFilePath
  printf "Generated default configuration at %s" configFilePath
mainWithArgs ("runconf":fuzzParamsPath:_) =
  readFuzzParams fuzzParamsPath >>= fuzz
mainWithArgs ("genexpr":configFilePath:_) = do
  putStrLn "Generating expression:"
  params <- readFuzzParams configFilePath
  randomExprs params >>= print
mainWithArgs _ =
  error
    "Usage: \n\
    \       Fuzz genconf <fuzz.json>\n\
    \       Fuzz runconf <fuzz.json>\n\
    \       Fuzz genexpr <fuzz.json>"

main :: IO ()
main = getArgs >>= mainWithArgs
