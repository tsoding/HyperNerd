{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Effect
  ( Effect
  , EffectF(..)
  , Selector(..)
  , Condition(..)
  , Order(..)
  , say
  , logMsg
  , createEntity
  , getEntityById
  , deleteEntityById
  , updateEntityById
  , selectEntities
  , deleteEntities
  , updateEntities
  , httpRequest
  , now
  , timeout
  , errorEff
  , twitchApiRequest
  , listen
  , periodicEffect
  , twitchCommand
  , randomMarkov
  ) where

import Control.Monad.Catch
import Control.Monad.Free
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import Data.Time
import Entity
import Network.HTTP.Simple (Request, Response)
import Property
import Transport

data Condition
  = PropertyEquals T.Text
                   Property
  | ConditionAnd [Condition]
  deriving (Show)

data Order
  = Asc
  | Desc
  deriving (Show)

data Selector
  = All
  | Filter Condition
           Selector
  | Shuffle Selector
  | Take Int
         Selector
  | SortBy T.Text
           Order
           Selector
  deriving (Show)

data EffectF s
  = Say Channel
        T.Text
        s
  | LogMsg T.Text
           s
  | ErrorEff T.Text
  | CreateEntity T.Text
                 Properties
                 (Entity Properties -> s)
  | GetEntityById T.Text
                  Int
                  (Maybe (Entity Properties) -> s)
  | DeleteEntityById T.Text
                     Int
                     s
  | UpdateEntityById (Entity Properties)
                     (Maybe (Entity Properties) -> s)
  | SelectEntities T.Text
                   Selector
                   ([Entity Properties] -> s)
  | DeleteEntities T.Text
                   Selector
                   (Int -> s)
  | UpdateEntities T.Text
                   Selector
                   Properties
                   (Int -> s)
  | Now (UTCTime -> s)
  | HttpRequest Request
                (Response B8.ByteString -> s)
  | TwitchApiRequest Channel
                     Request
                     (Response B8.ByteString -> s)
  | Timeout Integer
            (Effect ())
            s
  | Listen (Effect ())
           ([T.Text] -> s)
  | TwitchCommand Channel
                  T.Text
                  [T.Text]
                  s
  | RandomMarkov (Maybe T.Text -> s)
  deriving (Functor)

type Effect = Free EffectF

instance MonadThrow Effect where
  throwM :: Exception e => e -> Effect a
  throwM = errorEff . T.pack . displayException

say :: Channel -> T.Text -> Effect ()
say channel msg = liftF $ Say channel msg ()

logMsg :: T.Text -> Effect ()
logMsg msg = liftF $ LogMsg msg ()

-- TODO(#235): the result of createEntity effect is always ignored
createEntity :: IsEntity e => e -> Effect (Entity e)
createEntity entity =
  liftF
    (CreateEntity (nameOfEntity entity) (toProperties entity) id) >>=
  fromEntityProperties

getEntityById :: IsEntity e => T.Text -> Int -> Effect (Maybe (Entity e))
getEntityById name ident =
  fmap (>>= fromEntityProperties) $ liftF $ GetEntityById name ident id

deleteEntityById :: T.Text -> Int -> Effect ()
deleteEntityById name ident = liftF $ DeleteEntityById name ident ()

updateEntityById :: IsEntity e => Entity e -> Effect (Maybe (Entity e))
updateEntityById entity =
  fmap (>>= fromEntityProperties) $
  liftF $ UpdateEntityById (toProperties <$> entity) id

selectEntities :: IsEntity e => T.Text -> Selector -> Effect [Entity e]
selectEntities name selector =
  fmap (>>= fromEntityProperties) $ liftF $ SelectEntities name selector id

deleteEntities :: T.Text -> Selector -> Effect Int
deleteEntities name selector = liftF $ DeleteEntities name selector id

updateEntities :: T.Text -> Selector -> Properties -> Effect Int
updateEntities name selector properties =
  liftF $ UpdateEntities name selector properties id

now :: Effect UTCTime
now = liftF $ Now id

httpRequest :: Request -> Effect (Response B8.ByteString)
httpRequest request = liftF $ HttpRequest request id

twitchApiRequest :: Channel -> Request -> Effect (Response B8.ByteString)
twitchApiRequest channel request = liftF $ TwitchApiRequest channel request id

timeout :: Integer -> Effect () -> Effect ()
timeout t e = liftF $ Timeout t e ()

errorEff :: T.Text -> Effect a
errorEff t = liftF $ ErrorEff t

listen :: Effect () -> Effect [T.Text]
listen effect = liftF $ Listen effect id

periodicEffect :: Integer -> Effect () -> Effect ()
periodicEffect period effect = do
  effect
  timeout period $ periodicEffect period effect

twitchCommand :: Channel -> T.Text -> [T.Text] -> Effect ()
twitchCommand channel name args = liftF $ TwitchCommand channel name args ()

randomMarkov :: Effect (Maybe T.Text)
randomMarkov = liftF $ RandomMarkov id
