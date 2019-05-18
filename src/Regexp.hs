{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Regexp
  ( regexParseArgs
  ) where

import Data.Array
import qualified Data.Text as T
import Text.InterpolatedString.QM
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

regexParseArgs :: T.Text -> T.Text -> Either String [T.Text]
regexParseArgs regexString textArgs = do
  let stringArgs = T.unpack textArgs
  regex <- compile defaultCompOpt defaultExecOpt $ T.unpack regexString
  result <- execute regex stringArgs
  case result of
    Just matches ->
      case map (T.pack . flip Regex.extract stringArgs) $ elems matches of
        _:finalArgs -> Right finalArgs
        [] -> Left "Not enough arguments"
    -- TODO: regexParseArgs has too specific error messages
    --   It mentions commands, but can be used not only for commands
    Nothing -> Left [qms|Command doesn't match '{regexString}' regex|]
