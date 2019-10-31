{-# LANGUAGE OverloadedStrings #-}

module Bot.Expr where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Data.Tuple

import HyperNerd.Parser

data Expr
  = TextExpr T.Text
  | FunCallExpr T.Text
                [Expr]
  deriving (Eq, Show)

type NameTable = ()

symbol :: Parser T.Text
symbol = notNull "Symbol name cannot be empty" $ takeWhileP isAlphaNum

stringLiteral :: Parser Expr
stringLiteral = do
  _ <- charP '"'
  value <- takeWhileP (/= '"')
  _ <- charP '"'
  return $ TextExpr value

funCallArg :: Parser Expr
funCallArg = funCall <|> stringLiteral

funCallArgList :: Parser [Expr]
funCallArgList = do
  _ <- charP '(' <* whitespaces
  args <-
    sepBy funCallArg (whitespaces >> charP ',' >> whitespaces) <|> return []
  _ <- whitespaces >> charP ')'
  return args

funCall :: Parser Expr
funCall = do
  name <- charP '%' *> symbol
  args <- funCallArgList <|> return []
  return $ FunCallExpr name args

whitespaces :: Parser T.Text
whitespaces = takeWhileP isSpace

textBlock :: Parser Expr
textBlock =
  Parser $ \input ->
    case T.uncons input of
      Nothing -> Left EOF
      Just ('%', input') ->
        return $ fmap (TextExpr . T.cons '%') $ swap $ T.span (/= '%') input'
      _ -> return $ fmap TextExpr $ swap $ T.span (/= '%') input

expr :: Parser Expr
expr = funCall <|> textBlock

exprs :: Parser [Expr]
exprs = normalizeExprs <$> many expr
  where
    normalizeExprs :: [Expr] -> [Expr]
    normalizeExprs [] = []
    normalizeExprs (TextExpr t1:TextExpr t2:rest) =
      normalizeExprs (TextExpr (t1 <> t2) : rest)
    normalizeExprs (x:rest) = x : normalizeExprs rest

-- TODO(#600): interpretExprs is not implemented
-- interpretExprs :: NameTable -> [Expr] -> Effect T.Text
-- interpretExprs = undefined
