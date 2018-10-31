{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Entity where

import Control.Monad.Catch
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Property

type Properties = M.Map T.Text Property

data Entity a = Entity
  { entityId :: Int
  , entityName :: T.Text
  , entityPayload :: a
  } deriving (Eq, Show, Generic, Functor)

fromEntityProperties ::
     (IsEntity e, MonadThrow m) => Entity Properties -> m (Entity e)
fromEntityProperties entity = do
  payload <- fromProperties (entityPayload entity)
  return $ fmap (const payload) entity

extractProperty :: (IsProperty a, MonadThrow m) => T.Text -> Properties -> m a
extractProperty fieldName properties = do
  property <-
    maybe
      (throwM $ PropertyNotFound fieldName)
      return
      (M.lookup fieldName properties)
  fromProperty property

class IsEntity e where
  toProperties :: e -> Properties
  fromProperties :: MonadThrow m => Properties -> m e

restoreEntity ::
     T.Text
  -> Int
  -> [(T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime)]
  -> Maybe (Entity Properties)
restoreEntity name ident rawProperties = do
  let properties = mapMaybe restoreProperty rawProperties
  if null properties
    then Nothing
    else Just
           Entity
             { entityName = name
             , entityId = ident
             , entityPayload = M.fromList properties
             }

instance IsEntity Properties where
  toProperties = id
  fromProperties = return
