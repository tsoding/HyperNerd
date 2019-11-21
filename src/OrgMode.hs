{-# LANGUAGE OverloadedStrings #-}

module OrgMode
  ( renderTable
  ) where

import Data.List
import qualified Data.Text as T

charEscapeList :: String
charEscapeList = "|"

renderTable :: [T.Text] -> [[T.Text]] -> T.Text
renderTable header rows =
  T.unlines ([renderRow header, "|-"] <> map (renderRow . normalizeRow) rows)
  where
    normalizeRow row = take (length header) (row ++ repeat "")
    renderRow :: [T.Text] -> T.Text
    renderRow columns =
      "|" <> T.concat (intersperse "|" $ map escapeColumn columns) <> "|"
    escapeColumn :: T.Text -> T.Text
    escapeColumn = T.filter $ not . (`elem` charEscapeList)
