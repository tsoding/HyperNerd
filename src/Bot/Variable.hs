{-# LANGUAGE OverloadedStrings #-}
module Bot.Variable ( expandVariables
                    , addVariable
                    , deleteVariable
                    , updateVariable
                    ) where

import           Command
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Entity
import           Property

data Variable = Variable { variableName :: T.Text
                         , variableValue :: T.Text
                         } deriving Show

instance IsEntity Variable where
    toProperties variable =
        M.fromList [ ("name", PropertyText $ variableName variable)
                   , ("value", PropertyText $ variableValue variable)
                   ]
    fromProperties properties =
        Variable <$> extractProperty "name" properties
                 <*> extractProperty "value" properties

-- TODO(#243): expandVariables is not implemented
expandVariables :: T.Text -> Effect T.Text
expandVariables = return

-- TODO(#244): addVariable is not implemented
addVariable :: CommandHandler T.Text
addVariable _ = return ()

-- TODO(#245): deleteVariable is not implemented
deleteVariable :: CommandHandler T.Text
deleteVariable _ = return ()

-- TODO(#246): updateVariable is not implemented
updateVariable :: CommandHandler (T.Text, T.Text)
updateVariable _ = return ()
