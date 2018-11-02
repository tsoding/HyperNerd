{-# LANGUAGE OverloadedStrings #-}

module Property where

import Control.Monad.Catch
import qualified Data.Text as T
import Data.Time

data PropertyException
  = PropertyNotFound T.Text
  | UnexpectedPropertyType T.Text
  deriving (Show)

instance Exception PropertyException

data Property
  = PropertyInt Int
  | PropertyText T.Text
  | PropertyUTCTime UTCTime
  deriving (Eq, Show)

class IsProperty a where
  fromProperty :: MonadThrow m => Property -> m a

instance IsProperty Int where
  fromProperty (PropertyInt x) = return x
  fromProperty _ = throwM $ UnexpectedPropertyType "Int"

instance IsProperty T.Text where
  fromProperty (PropertyText x) = return x
  fromProperty _ = throwM $ UnexpectedPropertyType "Text"

instance IsProperty UTCTime where
  fromProperty (PropertyUTCTime x) = return x
  fromProperty _ = throwM $ UnexpectedPropertyType "UTCTime"

propertyTypeName :: Property -> String
propertyTypeName (PropertyInt _) = "PropertyInt"
propertyTypeName (PropertyText _) = "PropertyText"
propertyTypeName (PropertyUTCTime _) = "PropertyUTCTime"

restoreProperty ::
     (T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime)
  -> Maybe (T.Text, Property)
restoreProperty (name, "PropertyInt", Just x, _, _) = Just (name, PropertyInt x)
restoreProperty (name, "PropertyText", _, Just x, _) =
  Just (name, PropertyText x)
restoreProperty (name, "PropertyUTCTime", _, _, Just x) =
  Just (name, PropertyUTCTime x)
restoreProperty _ = Nothing
