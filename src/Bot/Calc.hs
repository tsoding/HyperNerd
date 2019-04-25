{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Calc
  ( calcCommand
  ) where

import Bot.Replies
import Data.Char (isDigit)
import Data.Either.Extra
import qualified Data.Text as T
import Reaction
import Safe
import Text.InterpolatedString.QM
import Transport
import Data.Foldable

data Token
  = NumberToken Int
  | PlusToken
  | MinusToken
  deriving (Eq, Show)

tokenize :: T.Text -> Either String [Token]
tokenize (T.uncons -> Just (' ', xs)) = tokenize xs
tokenize (T.uncons -> Just ('+', xs)) = (PlusToken :) <$> tokenize xs
tokenize (T.uncons -> Just ('-', xs)) = (MinusToken :) <$> tokenize xs
-- TODO(#569): Multiplication operation is not supported by !calc
tokenize (T.uncons -> Just ('*', _)) =
  Left "https://github.com/tsoding/HyperNerd/issues/569"
-- TODO(#570): Division operation is not supported by !calc
tokenize (T.uncons -> Just ('/', _)) =
  Left "https://github.com/tsoding/HyperNerd/issues/570"
-- TODO(#574): !calc does not support fractional numbers
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
    (digits, rest) = T.span isDigit xs
tokenize (T.uncons -> Nothing) = return []
tokenize _ = Left "Error 😡"

infixToRpn :: Maybe Token -> [Token] -> Either String [Token]
infixToRpn Nothing (NumberToken x:rest) =
  (NumberToken x :) <$> infixToRpn Nothing rest
infixToRpn (Just op) (NumberToken x:rest) =
  (NumberToken x :) . (op :) <$> infixToRpn Nothing rest
infixToRpn Nothing (op:rest) = infixToRpn (Just op) rest
infixToRpn Nothing [] = return []
infixToRpn _ _ = Left "Error 😡"

type RpnState = [Int]

interpretToken :: RpnState -> Token -> Either String RpnState
interpretToken s (NumberToken x) = return (x:s)
interpretToken (x1:x2:xs) PlusToken = return (x2 + x1:xs)
interpretToken (x1:x2:xs) MinusToken = return (x2 - x1:xs)
interpretToken _ _ = Left "Error 😡"

interpretRpn :: [Token] -> Either String Int
interpretRpn tokens = do
  result <- foldlM interpretToken [] tokens
  case result of
    [x] -> return x
    _   -> Left "Error 😡"

calc :: T.Text -> Either String Int
calc text = do
  tokens <- tokenize text
  rpn <- infixToRpn Nothing tokens
  interpretRpn rpn

calcCommand :: Reaction Message T.Text
calcCommand =
  cmapR calc $ replyLeft $ cmapR (T.pack . show) $ Reaction replyMessage
