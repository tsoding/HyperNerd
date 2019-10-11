{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Expr
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.List
import qualified Data.Text as T
import System.Environment
import System.Random
import Text.Printf

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
unparseExpr (VarExpr name) = "%" <> name
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

normalizeExprs :: [Expr] -> [Expr]
normalizeExprs [] = []
normalizeExprs (TextExpr t1:TextExpr t2:rest) =
  normalizeExprs (TextExpr (t1 <> t2) : rest)
normalizeExprs (_:rest) = normalizeExprs rest

randomExprs :: FuzzParams -> IO [Expr]
randomExprs params = do
  n <- randomRIO $ fpExprsRange params
  replicateM n (randomExpr params)

fuzzIteration :: FuzzParams -> IO Bool
fuzzIteration params = do
  es <- normalizeExprs <$> randomExprs params
  let es' = runParser exprs $ unparseExprs es
  when (Right ("", es) /= es') $ do
    print es
    print es'
    error "test"
  return (Right ("", es) == es')

fuzz :: FuzzParams -> IO ()
fuzz params = do
  report <- replicateM (fpFuzzCount params) (fuzzIteration params)
  printf "Failures: %d\n" $ length $ filter not report
  printf "Successes: %d\n" $ length $ filter id report

mainWithArgs :: [String] -> IO ()
mainWithArgs (fuzzParamsPath:_) = readFuzzParams fuzzParamsPath >>= fuzz
mainWithArgs _ = error "Usage: Fuzz <fuzz.json>"

main :: IO ()
main = getArgs >>= mainWithArgs
