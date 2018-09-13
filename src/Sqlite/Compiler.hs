{-# LANGUAGE OverloadedStrings #-}
module Sqlite.Compiler where

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Effect
import           Text.Printf

-- TODO(#250): compileCteChain is not implemented
compileCteChain :: Selector -> (T.Text, Query, [NamedParam])
compileCteChain = undefined

compileSelector :: Selector -> (Query, [NamedParam])
compileSelector selector =
    ( Query $ T.unlines [ header, fromQuery cteChainQuery, footer ]
    , namedParams
    )
    where header = "with"
          footer = T.pack $ printf "select * from %s" rootCte
          (rootCte, cteChainQuery, namedParams) = compileCteChain selector
