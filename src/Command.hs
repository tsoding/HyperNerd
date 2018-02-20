{-# LANGUAGE ViewPatterns #-}
module Command where

import           Data.Char
import qualified Data.Text as T

data Command a = Command { commandName :: T.Text
                         , commandArgs :: a
                         } deriving (Eq, Show)

textAsCommand :: T.Text -> Maybe (Command T.Text)
textAsCommand (T.uncons -> Just ('!', restText)) =
    Just $ Command { commandName = T.takeWhile isAlphaNum restText
                   , commandArgs = T.dropWhile isSpace $ T.dropWhile isAlphaNum restText
                   }
textAsCommand _ = Nothing
