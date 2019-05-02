module Bot.Expr (parseExprs, interpretExprs) where

import qualified Data.Text as T
import Effect

data Expr = TextExpr T.Text
          | FunCallExpr T.Text [Expr]
          | VarExpr T.Text
            deriving (Eq, Show)

-- TODO: parseExprs is not implemented
parseExprs :: T.Text -> Either String [Expr]
parseExprs = undefined

-- TODO: interpretExprs is not implemented
interpretExprs :: [Expr] -> Effect T.Text
interpretExprs = undefined
