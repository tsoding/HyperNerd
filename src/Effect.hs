{-# LANGUAGE DeriveFunctor #-}
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
              , redirectSay
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
               | UpdateEntityById (Entity Properties) (Maybe (Entity Properties) -> s)
               | SelectEntities T.Text Selector ([Entity Properties] -> s)
               | DeleteEntities T.Text Selector (Int -> s)
               | UpdateEntities T.Text Selector Properties (Int -> s)
               | Now (UTCTime -> s)
               | HttpRequest Request (Response B8.ByteString -> s)
               | TwitchApiRequest Request (Response B8.ByteString -> s)
               | Timeout Integer (Effect ()) s
               | RedirectSay (Effect ()) ([T.Text] -> s)
                 deriving Functor

type Effect = Free EffectF

instance MonadThrow Effect where
    throwM :: Exception e => e -> Effect a
    throwM = errorEff . T.pack . displayException

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()

logMsg :: T.Text -> Effect ()
logMsg msg = liftF $ LogMsg msg ()

-- TODO: the result of createEntity effect is always ignored
createEntity :: IsEntity e => T.Text -> e -> Effect (Entity e)
createEntity name entity =
    liftF (CreateEntity name (toProperties entity) id) >>= fromEntityProperties

-- TODO(#187): getEntityById should have type `IsEntity e => T.Text -> Int -> Effect (Maybe Entity e)`
getEntityById :: T.Text -> Int -> Effect (Maybe (Entity Properties))
getEntityById name ident = liftF $ GetEntityById name ident id

updateEntityById :: IsEntity e => Entity e -> Effect (Maybe (Entity e))
updateEntityById entity =
    fmap (>>= fromEntityProperties) $ liftF $ UpdateEntityById (toProperties <$> entity) id

-- TODO(#188): selectEntities shoudl have type `IsEntity e => T.Text -> Selector -> Effect [Entity e]`
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

redirectSay :: Effect () -> Effect [T.Text]
redirectSay effect = liftF $ RedirectSay effect id
