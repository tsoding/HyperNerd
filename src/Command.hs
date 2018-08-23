{-# LANGUAGE ViewPatterns #-}
module Command where

import           Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Events

type CommandHandler a = Sender -> a -> Effect ()
type CommandTable a = M.Map T.Text (T.Text, CommandHandler a)

data Command a = Command { commandName :: T.Text
                         , commandArgs :: a
                         } deriving (Eq, Show)

textAsCommand :: T.Text -> Maybe (Command T.Text)
textAsCommand (T.uncons -> Just ('!', restText)) =
    Just Command { commandName = T.takeWhile isAlphaNum restText
                 , commandArgs = T.dropWhile isSpace $ T.dropWhile isAlphaNum restText
                 }
textAsCommand _ = Nothing

textAsPipe :: T.Text -> [Command T.Text]
textAsPipe _ = []
