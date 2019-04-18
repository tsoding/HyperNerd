{-# LANGUAGE ViewPatterns #-}

module Bot.Calc
  ( calcCommand
  ) where

import Bot.Replies
import qualified Data.Text as T
import Reaction
import Transport
import Data.Char (isDigit)
import Safe
import Data.Either.Extra

data Expr = NumberExpr Int
          | PlusExpr Expr Expr
            deriving (Eq, Show)

data Token = NumberToken Int
           | PlusToken
             deriving (Eq, Show)

-- TODO(#567): !calc Int overflow is not reported as an error
-- TODO(#568): Minusation operation is not supported by !calc
-- TODO(#569): Multiplication operation is not supported by !calc
-- TODO(#570): Division operation is not supported by !calc
-- TODO(#571): Parenthesis are not supported by !calc
-- TODO: !calc produce vague syntax error reports
-- TODO: !calc does not support negative numbers
-- TODO: !calc does not support fractional numbers

tokenize :: T.Text -> Either String [Token]
tokenize (T.uncons -> Just(' ', xs)) = tokenize xs
tokenize (T.uncons -> Just ('+', xs)) = (PlusToken :) <$> tokenize xs
tokenize xs@(T.uncons -> Just (x, _))
  | isDigit x = do
    token <- NumberToken <$> maybeToEither "Error 😡" (readMay $ T.unpack digits)
    (token :) <$> tokenize rest
  where
    (digits, rest) = T.span isDigit xs
tokenize (T.uncons -> Nothing) = return []
tokenize _ = Left "Error 😡"

parseExpr :: [Token] -> Either String Expr
parseExpr [NumberToken x] = Right $ NumberExpr x
parseExpr (NumberToken x:PlusToken:rest) =
  PlusExpr (NumberExpr x) <$> parseExpr rest
parseExpr _ = Left "Error 😡"

interpretExpr :: Expr -> Int
interpretExpr (NumberExpr x) = x
interpretExpr (PlusExpr a b) = interpretExpr a + interpretExpr b

calc :: T.Text -> Either String Int
calc text = do
  tokens <- tokenize text
  expr <- parseExpr tokens
  return $ interpretExpr expr

calcCommand :: Reaction Message T.Text
calcCommand =
  cmapR calc $ replyLeft $ cmapR (T.pack . show) $ Reaction replyMessage
