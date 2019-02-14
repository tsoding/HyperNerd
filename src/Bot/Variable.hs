{-# LANGUAGE OverloadedStrings #-}

module Bot.Variable
  ( expandVariables
  , addVariable
  , deleteVariable
  , updateVariable
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Effect
import Entity
import Transport
import Property
import Reaction

data Variable = Variable
  { variableName :: T.Text
  , variableValue :: T.Text
  } deriving (Show)

instance IsEntity Variable where
  toProperties variable =
    M.fromList
      [ ("name", PropertyText $ variableName variable)
      , ("value", PropertyText $ variableValue variable)
      ]
  fromProperties properties =
    Variable <$> extractProperty "name" properties <*>
    extractProperty "value" properties

-- TODO(#243): expandVariables is not implemented
expandVariables :: T.Text -> Effect T.Text
expandVariables = return

-- TODO(#244): addVariable is not implemented
addVariable :: Reaction Message T.Text
addVariable = ignore

-- TODO(#245): deleteVariable is not implemented
deleteVariable :: Reaction Message T.Text
deleteVariable = ignore

-- TODO(#246): updateVariable is not implemented
updateVariable :: Reaction Message (T.Text, T.Text)
updateVariable = ignore
