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
  | VarExpr T.Text
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

funcallarg :: Parser Expr
funcallarg = funcall <|> var <|> stringLiteral

funcall :: Parser Expr
funcall = do
  _ <- charP '%'
  name <- symbol
  _ <- whitespaces >> charP '(' >> whitespaces
  args <-
    sepBy funcallarg (whitespaces >> charP ',' >> whitespaces) <|> return []
  _ <- whitespaces >> charP ')'
  return $ FunCallExpr name args

whitespaces :: Parser T.Text
whitespaces = takeWhileP isSpace

var :: Parser Expr
var = charP '%' *> (VarExpr <$> symbol) <* charP '%'

textBlock :: Parser Expr
textBlock =
  Parser $ \input ->
    case T.uncons input of
      Nothing -> Left EOF
      Just ('%', _) -> Left (SyntaxError "Text block does not start with %")
      _ -> return $ fmap TextExpr $ swap $ T.span (/= '%') input

expr :: Parser Expr
expr = funcall <|> var <|> textBlock

exprs :: Parser [Expr]
exprs = many expr
-- TODO(#600): interpretExprs is not implemented
-- interpretExprs :: NameTable -> [Expr] -> Effect T.Text
-- interpretExprs = undefined
