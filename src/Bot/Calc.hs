{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Calc
  ( calcCommand
  , supportedOps
  ) where

import Bot.Replies
import Data.Char (isDigit)
import Data.Either.Extra
import Data.Foldable
import qualified Data.Text as T
import Reaction
import Safe
import Text.InterpolatedString.QM
import Transport

data Op
  = Plus
  | Minus
  | UnaryMinus
  | Multiply
  | Division
  | Exp
  deriving (Eq, Show, Enum, Bounded)

data Token
  = NumberToken Double
  | OpToken Op
  deriving (Eq, Show)

opName :: Op -> T.Text
opName Plus = "+"
opName Minus = "-"
opName UnaryMinus = "-"
opName Multiply = "*"
opName Division = "/"
opName Exp = "^"

supportedOps :: [T.Text]
supportedOps = map opName [minBound :: Op .. maxBound]

-- TODO(#618): !calc does not support scientific notation
tokenize :: T.Text -> Either String [Token]
tokenize (T.uncons -> Just (' ', xs)) = tokenize xs
tokenize (T.uncons -> Just ('+', xs)) = (OpToken Plus :) <$> tokenize xs
tokenize (T.uncons -> Just ('-', xs)) = (OpToken Minus :) <$> tokenize xs
tokenize (T.uncons -> Just ('*', xs)) = (OpToken Multiply :) <$> tokenize xs
tokenize (T.uncons -> Just ('/', xs)) = (OpToken Division :) <$> tokenize xs
-- TODO(#619): Mod operation is not supported anymore by !calc
--   Because `Data.Fixed.mod' 100.0 0.0` throws an Exception that
--   somehow short-circuits the supavisah
tokenize (T.uncons -> Just ('%', _)) = Left "Mod operation is disable for now."
tokenize (T.uncons -> Just ('^', xs)) = (OpToken Exp :) <$> tokenize xs
tokenize (T.uncons -> Just ('.', _)) =
  Left "https://github.com/tsoding/HyperNerd/issues/574"
-- TODO(#571): Parenthesis are not supported by !calc
-- TODO(#573): !calc does not support negative numbers
-- TODO(#567): !calc Int overflow is not reported as an error
tokenize xs@(T.uncons -> Just (x, _))
  | x `elem` ['(', ')'] = Left "https://github.com/tsoding/HyperNerd/issues/571"
  | isDigit x = do
    token <-
      NumberToken <$>
      maybeToEither
        [qms|{digits} does not look like a number|]
        (readMay $ T.unpack digits)
    (token :) <$> tokenize rest
  | otherwise = Left [qms|I don't know what's this `{x}`|]
  where
    (digits, rest) = T.span (\a -> isDigit a || a == '.') xs
tokenize (T.uncons -> Nothing) = return []
tokenize _ = Left "Tokenizer Error 😡"

urinateMinus :: [Token] -> [Token]
urinateMinus (NumberToken x:OpToken Minus:restTokens) =
  NumberToken x : OpToken Minus : urinateMinus restTokens
urinateMinus (OpToken Minus:restTokens) =
  OpToken UnaryMinus : urinateMinus restTokens
urinateMinus (token:restTokens) = token : urinateMinus restTokens
urinateMinus [] = []

precedence :: Op -> Int
precedence Plus = 0
precedence Minus = 0
precedence Multiply = 1
precedence Division = 1
precedence Exp = 2
precedence UnaryMinus = 3

infixToRpn :: [Op] -> [Token] -> Either String [Token]
infixToRpn opStack (NumberToken x:restTokens) =
  (NumberToken x :) <$> infixToRpn opStack restTokens
infixToRpn [] (OpToken op:rest) = infixToRpn [op] rest
infixToRpn opStack@(op0:_) (OpToken op1:rest)
  | precedence op0 <= precedence op1 = infixToRpn (op1 : opStack) rest
  | otherwise =
    (map OpToken outputOps ++) <$> infixToRpn (op1 : restOpStack) rest
  where
    (outputOps, restOpStack) =
      span (\opx -> precedence opx >= precedence op1) opStack
infixToRpn opStack [] = return $ map OpToken opStack

type RpnState = [Double]

interpretToken :: RpnState -> Token -> Either String RpnState
interpretToken s (NumberToken x) = return (x : s)
interpretToken (x:xs) (OpToken UnaryMinus) = return ((-x) : xs)
interpretToken (x1:x2:xs) (OpToken Plus) = return ((x2 + x1) : xs)
interpretToken (x1:x2:xs) (OpToken Minus) = return ((x2 - x1) : xs)
interpretToken (x1:x2:xs) (OpToken Multiply) = return ((x2 * x1) : xs)
interpretToken (x1:x2:xs) (OpToken Division) = return ((x2 / x1) : xs)
interpretToken (x1:x2:xs) (OpToken Exp) = return ((x2 ** x1) : xs)
interpretToken _ _ = Left "Interpreter Error"

interpretRpn :: [Token] -> Either String Double
interpretRpn tokens = do
  result <- foldlM interpretToken [] tokens
  case result of
    [x] -> return x
    _ -> Left "Final Stack Size is not equal to 1 😡"

calc :: T.Text -> Either String Double
calc text = do
  tokens <- urinateMinus <$> tokenize text
  rpn <- infixToRpn [] tokens
  interpretRpn rpn

calcCommand :: Reaction Message T.Text
calcCommand =
  cmapR calc $ replyLeft $ cmapR (T.pack . show) $ Reaction replyMessage
