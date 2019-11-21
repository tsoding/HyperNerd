{-# LANGUAGE OverloadedStrings #-}
module OrgMode (renderTable) where

import qualified Data.Text as T
import Data.List

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
