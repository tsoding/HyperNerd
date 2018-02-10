module Effect ( Effect
              , EffectF (..)
              , say
              , saveEntity
              , getEntityById
              , now
              ) where

import Control.Monad.Free
import qualified Data.Text as T
import Data.Time
import Entity

data EffectF s = Say T.Text s
               | SaveEntity T.Text Properties (Entity -> s)
               | GetEntityById T.Text Int (Maybe Entity -> s)
               | Now (UTCTime -> s)

instance Functor EffectF where
    fmap f (Say msg s)      = Say msg (f s)
    fmap f (SaveEntity name properties h) =
        SaveEntity name properties (f . h)
    fmap f (GetEntityById name ident h) =
        GetEntityById name ident (f . h)
    fmap f (Now h) = Now (f . h)

type Effect = Free EffectF

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()

saveEntity :: T.Text -> Properties -> Effect Entity
saveEntity name properties = liftF $ SaveEntity name properties id

getEntityById :: T.Text -> Int -> Effect (Maybe Entity)
getEntityById name ident = liftF $ GetEntityById name ident id

now :: Effect UTCTime
now = liftF $ Now id
