{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Entity where

import           Control.Monad.Catch
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Property
import           Text.Printf

type Properties = M.Map T.Text Property

data Entity a = Entity { entityId :: Int
                       , entityName :: T.Text
                       , entityPayload :: a
                       } deriving (Eq, Show)

extractProperty :: (IsProperty a, MonadThrow m) => T.Text -> Entity Properties -> m a
extractProperty fieldName entity =
    do property <- maybe (throwM $ PropertyException (T.pack $ printf "No field '%s' in entity '%s' with id %d"
                                                                      fieldName
                                                                      (entityName entity)
                                                                      (entityId entity)))
                   return
                   (M.lookup fieldName $ entityPayload entity)
       fromProperty property

class IsEntity e where
    toProperties :: e -> Properties
    fromEntity :: MonadThrow m => Entity Properties -> m e

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
