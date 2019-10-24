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
import qualified Data.ByteString.Lazy as BS

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
randomExprs params = do
  randomRIO (fpExprsRange params) >>= f []
  where
    normalizeExprs :: [Expr] -> [Expr]
    normalizeExprs [] = []
    normalizeExprs (TextExpr t1:TextExpr t2:rest) =
      normalizeExprs (TextExpr (t1 <> t2) : rest)
    normalizeExprs (x:rest) = x:normalizeExprs rest

    f :: [Expr] -> Int -> IO [Expr]
    f es n
      | m >= n = return es
      | otherwise = do
         es' <- replicateM (n - m) (randomExpr params)
         f (normalizeExprs (es ++ es')) n
      where m = length es

fuzzIteration :: FuzzParams -> IO Bool
fuzzIteration params = do
  es <- randomExprs params
  let es' = runParser exprs $ unparseExprs es
  when (Right ("", es) /= es') $ do
    print es
    print es'
    error "Failed"
  return (Right ("", es) == es')

fuzz :: FuzzParams -> IO ()
fuzz params = do
  report <- replicateM (fpFuzzCount params) (fuzzIteration params)
  printf "Failures: %d\n" $ length $ filter not report
  printf "Successes: %d\n" $ length $ filter id report

mainWithArgs :: [String] -> IO ()
mainWithArgs ("genconf":configFilePath:_) = do
  saveFuzzParams defaultFuzzParams configFilePath
  printf "Generated default configuration at %s" configFilePath
mainWithArgs ("runconf":fuzzParamsPath:_) = readFuzzParams fuzzParamsPath >>= fuzz
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
