{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Entity where

import           Control.Monad.Catch
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Text.Printf

newtype EntityException = EntityException T.Text deriving Show

instance Exception EntityException

data Property = PropertyInt Int
              | PropertyText T.Text
              | PropertyUTCTime UTCTime
                deriving (Eq, Show)

type Properties = M.Map T.Text Property

data Entity = Entity { entityId :: Int
                     , entityName :: T.Text
                     , entityProperties :: M.Map T.Text Property
                     } deriving (Eq, Show)


extractProperty :: (IsProperty a, MonadThrow m) => T.Text -> Entity -> m a
extractProperty fieldName entity =
    do property <- maybe (throwM $ EntityException (T.pack $ printf "No field '%s' in entity '%s' with id %d"
                                                                    fieldName
                                                                    (entityName entity)
                                                                    (entityId entity)))
                   return
                   (M.lookup fieldName $ entityProperties entity)
       fromProperty property

class IsEntity e where
    toProperties :: e -> Properties
    fromEntity :: MonadThrow m => Entity -> m e

class IsProperty a where
    fromProperty :: MonadThrow m => Property -> m a

instance IsProperty Int where
    fromProperty (PropertyInt x) = return  x
    fromProperty _ = throwM $ EntityException "Could parse a field. Expected type Int"

instance IsProperty T.Text where
    fromProperty (PropertyText x) = return x
    fromProperty _ = throwM $ EntityException "Could parse a field. Expected type Text"

instance IsProperty UTCTime where
    fromProperty (PropertyUTCTime x) = return x
    fromProperty _ = throwM $ EntityException "Could parse a field. Expected type UTCTime"

propertyTypeName :: Property -> String
propertyTypeName (PropertyInt _) = "PropertyInt"
propertyTypeName (PropertyText _) = "PropertyText"
propertyTypeName (PropertyUTCTime _) = "PropertyUTCTime"

restoreProperty :: (T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime) -> Maybe (T.Text, Property)
restoreProperty (name, "PropertyInt", Just x,      _, _) = Just (name, PropertyInt x)
restoreProperty (name, "PropertyText",     _, Just x, _) = Just (name, PropertyText x)
restoreProperty (name, "PropertyUTCTime",  _,      _, Just x) = Just (name, PropertyUTCTime x)
restoreProperty _ = Nothing

restoreEntity :: T.Text -> Int -> [(T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime)] -> Maybe Entity
restoreEntity name ident rawProperties =
    do properties <- return $ mapMaybe restoreProperty rawProperties
       if null properties
       then Nothing
       else Just Entity { entityName = name
                        , entityId = ident
                        , entityProperties = M.fromList properties
                        }
