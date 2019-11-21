{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module OrgMode (renderTable) where

import qualified Data.Text as T
import Data.List

charEscapeList :: [Char]
charEscapeList = "|"

renderTable :: [T.Text] -> [[T.Text]] -> T.Text
renderTable header rows =
  T.unlines ([renderRow header, "|-"] <> map (renderRow . normalizeRow) rows)
  where
    normalizeRow row = take (length header) (row ++ cycle [""])
    renderRow :: [T.Text] -> T.Text
    renderRow columns =
      "|" <> T.concat (intersperse "|" $ map escapeColumn columns) <> "|"
    escapeColumn :: T.Text -> T.Text
    escapeColumn =
      T.concatMap
        (\x ->
           if x `elem` charEscapeList
             then T.pack ['\\', x]
             else T.singleton x)
