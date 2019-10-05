{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Expr where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Data.Tuple
import Effect

-- import Control.Monad
data Expr
  = TextExpr T.Text
  | FunCallExpr T.Text
                [Expr]
  | VarExpr T.Text
  deriving (Eq, Show)

type NameTable = ()

data ParserStop
  = EOF
  | SyntaxError T.Text
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: T.Text -> Either ParserStop (T.Text, a)
  } deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \text -> Right (text, x)
  (Parser f) <*> (Parser x) =
    Parser $ \input1 -> do
      (input2, f') <- f input1
      (input3, x') <- x input2
      return (input3, f' x')

instance Monad Parser where
  Parser a >>= f =
    Parser $ \input1 -> do
      (input2, b) <- a input1
      runParser (f b) input2

instance Alternative Parser where
  empty = Parser $ const $ Left EOF
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      case (p1 input, p2 input) of
        (Left _, x) -> x
        (x, _) -> x

symbol :: Parser T.Text
symbol = notNull "Symbol name cannot be empty" $ takeWhileP isAlphaNum

stringLiteral :: Parser Expr
stringLiteral = do
  _ <- charP '"'
  value <- takeWhileP (/= '"')
  _ <- charP '"'
  return $ TextExpr value

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element sep = do
  arg <- element
  args <- many (sep >> element)
  return (arg : args)

funcall :: Parser Expr
funcall = do
  _ <- charP '%' >> whitespaces
  name <- symbol
  _ <- whitespaces >> charP '(' >> whitespaces
  args <-
    sepBy (funcall <|> var <|> stringLiteral) (whitespaces >> charP ',') <|>
    return []
  _ <- whitespaces >> charP ')'
  return $ FunCallExpr name args

charP :: Char -> Parser Char
charP a =
  Parser $ \input ->
    case T.uncons input of
      Just (b, rest)
        | a == b -> Right (rest, b)
      _ -> Left $ SyntaxError ("Expected `" <> T.pack [a] <> "`")

takeWhileP :: (Char -> Bool) -> Parser T.Text
takeWhileP p = Parser $ \input -> return $ swap $ T.span p input

syntaxError :: T.Text -> Parser a
syntaxError message = Parser $ \_ -> Left $ SyntaxError message

notNull :: T.Text -> Parser T.Text -> Parser T.Text
notNull message next =
  next >>=
  (\value ->
     if T.null value
       then syntaxError message
       else return value)

whitespaces :: Parser T.Text
whitespaces = takeWhileP isSpace

var :: Parser Expr
var = charP '%' >> whitespaces >> (VarExpr <$> symbol)

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
interpretExprs :: NameTable -> [Expr] -> Effect T.Text
interpretExprs = undefined
