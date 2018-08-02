{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Effect ( Effect
              , EffectF (..)
              , Selector (..)
              , Condition (..)
              , say
              , logMsg
              , createEntity
              , getEntityById
              , updateEntityById
              , selectEntities
              , deleteEntities
              , updateEntities
              , httpRequest
              , now
              , timeout
              , errorEff
              , twitchApiRequest
              ) where

import           Control.Monad.Catch
import           Control.Monad.Free
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import           Data.Time
import           Entity
import           Network.HTTP.Simple
import           Property

data Condition = PropertyEquals T.Text Property deriving Show

data Selector = All
              | Filter Condition Selector
              | Shuffle Selector
              | Take Int Selector
                deriving Show

data EffectF s = Say T.Text s
               | LogMsg T.Text s
               | ErrorEff T.Text
               | CreateEntity T.Text Properties (Entity Properties -> s)
               | GetEntityById T.Text Int (Maybe (Entity Properties) -> s)
               | UpdateEntityById T.Text Int Properties (Maybe (Entity Properties) -> s)
               | SelectEntities T.Text Selector ([Entity Properties] -> s)
               | DeleteEntities T.Text Selector (Int -> s)
               | UpdateEntities T.Text Selector Properties (Int -> s)
               | Now (UTCTime -> s)
               | HttpRequest Request (Response B8.ByteString -> s)
               | TwitchApiRequest Request (Response B8.ByteString -> s)
               | Timeout Integer (Effect ()) s

instance Functor EffectF where
    fmap f (Say msg s) = Say msg (f s)
    fmap f (LogMsg msg s) = LogMsg msg (f s)
    fmap f (CreateEntity name properties h) =
        CreateEntity name properties (f . h)
    fmap _ (ErrorEff text) = ErrorEff text
    fmap f (GetEntityById name ident h) =
        GetEntityById name ident (f . h)
    fmap f (UpdateEntityById name ident properties h) =
        UpdateEntityById name ident properties (f . h)
    fmap f (SelectEntities name selector h) =
        SelectEntities name selector (f . h)
    fmap f (DeleteEntities name selector h) =
        DeleteEntities name selector (f . h)
    fmap f (UpdateEntities name selector properties h) =
        UpdateEntities name selector properties (f . h)
    fmap f (Now h) = Now (f . h)
    fmap f (HttpRequest r h) = HttpRequest r (f . h)
    fmap f (TwitchApiRequest r h) = TwitchApiRequest r (f . h)
    fmap f (Timeout t e h) = Timeout t e (f h)

type Effect = Free EffectF

instance MonadThrow Effect where
    throwM :: Exception e => e -> Effect a
    throwM = errorEff . T.pack . displayException

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()

logMsg :: T.Text -> Effect ()
logMsg msg = liftF $ LogMsg msg ()

createEntity :: IsEntity e => T.Text -> e -> Effect (Entity Properties)
createEntity name entity =
    liftF $ CreateEntity name (toProperties entity) id

getEntityById :: T.Text -> Int -> Effect (Maybe (Entity Properties))
getEntityById name ident = liftF $ GetEntityById name ident id

updateEntityById :: T.Text -> Int -> Properties -> Effect (Maybe (Entity Properties))
updateEntityById name ident properties =
    liftF $ UpdateEntityById name ident properties id

selectEntities :: T.Text -> Selector -> Effect [Entity Properties]
selectEntities name selector = liftF $ SelectEntities name selector id

deleteEntities :: T.Text -> Selector -> Effect Int
deleteEntities name selector = liftF $ DeleteEntities name selector id

updateEntities :: T.Text -> Selector -> Properties -> Effect Int
updateEntities name selector properties = liftF $ UpdateEntities name selector properties id

now :: Effect UTCTime
now = liftF $ Now id

httpRequest :: Request -> Effect (Response B8.ByteString)
httpRequest request = liftF $ HttpRequest request id

twitchApiRequest :: Request -> Effect (Response B8.ByteString)
twitchApiRequest request = liftF $ TwitchApiRequest request id

timeout :: Integer -> Effect () -> Effect ()
timeout t e = liftF $ Timeout t e ()

errorEff :: T.Text -> Effect a
errorEff t = liftF $ ErrorEff t
