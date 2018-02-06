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
               | SaveEntity Entity (Int -> s)
               | GetEntityById T.Text Int (Maybe Entity -> s)
               | Now (UTCTime -> s)

instance Functor EffectF where
    fmap f (Say msg s)      = Say msg (f s)
    fmap f (SaveEntity entity h) =
        SaveEntity entity (f . h)
    fmap f (GetEntityById name id h) =
        GetEntityById name id (f . h)
    fmap f (Now h) = Now (f . h)

type Effect = Free EffectF

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()

saveEntity :: Entity -> Effect Int
saveEntity entity = liftF $ SaveEntity entity id

getEntityById :: T.Text -> Int -> Effect (Maybe Entity)
getEntityById name ident = liftF $ GetEntityById name ident id

now :: Effect UTCTime
now = liftF $ Now id
