{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Entity where

import           Control.Monad.Catch
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Property

type Properties = M.Map T.Text Property

data Entity a = Entity { entityId :: Int
                       , entityName :: T.Text
                       , entityPayload :: a
                       } deriving (Eq, Show, Generic, Functor)

extractProperty :: (IsProperty a, MonadThrow m) => T.Text -> Entity Properties -> m a
extractProperty fieldName entity =
    do property <- maybe (throwM $ PropertyNotFound fieldName)
                   return
                   (M.lookup fieldName $ entityPayload entity)
       fromProperty property

class IsEntity e where
    toProperties :: e -> Properties
    -- TODO(#189): fromProperties should have type `MonadThrow m => Properties -> m e`
    fromProperties :: MonadThrow m => Entity Properties -> m (Entity e)

restoreEntity :: T.Text
              -> Int
              -> [(T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime)]
              -> Maybe (Entity Properties)
restoreEntity name ident rawProperties =
    do properties <- return $ mapMaybe restoreProperty rawProperties
       if null properties
       then Nothing
       else Just Entity { entityName = name
                        , entityId = ident
                        , entityPayload = M.fromList properties
                        }

instance IsEntity Properties where
    toProperties = id
    fromProperties = return
