{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module HyperNerd.Parser where

import Control.Applicative
import qualified Data.Text as T
import Data.Tuple

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

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element sep = do
  arg <- element
  args <- many (sep >> element)
  return (arg : args)

takeWhileP :: (Char -> Bool) -> Parser T.Text
takeWhileP p = Parser $ \input -> return $ swap $ T.span p input

charP :: Char -> Parser Char
charP a =
  Parser $ \input ->
    case T.uncons input of
      Just (b, rest)
        | a == b -> Right (rest, b)
      _ -> Left $ SyntaxError ("Expected `" <> T.pack [a] <> "`")

notNull :: T.Text -> Parser T.Text -> Parser T.Text
notNull message next =
  next >>=
  (\value ->
     if T.null value
       then syntaxError message
       else return value)

syntaxError :: T.Text -> Parser a
syntaxError message = Parser $ \_ -> Left $ SyntaxError message
