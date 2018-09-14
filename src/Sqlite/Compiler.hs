{-# LANGUAGE OverloadedStrings #-}
module Sqlite.Compiler (compileSelector) where

import           Data.Monoid
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Effect
import           Text.Printf

type NamedQuery = (Query, [NamedParam])

header :: NamedQuery
header = ("with ", [])

footer :: Int -> NamedQuery
footer cteId = (Query $ T.pack $ printf " select * from t%d" cteId, [])

-- TODO(#250): compileCteChain is not implemented
compileCteChain :: T.Text -> Selector -> (Int, NamedQuery)
compileCteChain _ _ = undefined

compileSelector :: T.Text -> Selector -> NamedQuery
compileSelector name selector =
    header <> body <> footer cteId
    where (cteId, body) = compileCteChain name selector
