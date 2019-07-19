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
  , httpRequest
  , now
  , timeout
  , errorEff
  , twitchApiRequest
  , listen
  , periodicEffect
  , periodicEffect'
  , twitchCommand
  , randomMarkov
  , reloadMarkov
  , callFun
  , githubApiRequest
  , randomEff
  ) where

import Control.Monad.Catch
import Control.Monad.Free
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Entity
import Network.HTTP.Simple (Request, Response)
import Property
import Transport

data Condition
  = PropertyEquals T.Text
                   Property
  | PropertyGreater T.Text
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
  | Now (UTCTime -> s)
  | HttpRequest Request
                (Response B8.ByteString -> s)
  | TwitchApiRequest Request
                     (Response B8.ByteString -> s)
  | GitHubApiRequest Request
                     (Response B8.ByteString -> s)
  | TimeoutEff Integer
               (Maybe Channel)
               (Effect ())
               s
  | Listen (Effect ())
           ([T.Text] -> s)
  | TwitchCommand Channel
                  T.Text
                  [T.Text]
                  s
  | RandomMarkov (Maybe T.Text -> s)
  | ReloadMarkov (Maybe T.Text -> s)
  | GetVar T.Text
           (Maybe T.Text -> s)
  | CallFun T.Text
            [T.Text]
            (Maybe T.Text -> s)
  | RandomEff (Int, Int)
              (Int -> s)
  deriving (Functor)

type Effect = Free EffectF

instance MonadThrow Effect where
  throwM :: Exception e => e -> Effect a
  throwM = errorEff . T.pack . displayException

say :: Channel -> T.Text -> Effect ()
say channel msg = liftF $ Say channel msg ()

logMsg :: T.Text -> Effect ()
logMsg msg = liftF $ LogMsg msg ()

createEntity :: IsEntity e => Proxy e -> e -> Effect (Entity e)
createEntity proxy entity =
  liftF (CreateEntity (nameOfEntity proxy) (toProperties entity) id) >>=
  fromEntityProperties

getEntityById :: IsEntity e => Proxy e -> Int -> Effect (Maybe (Entity e))
getEntityById proxy ident =
  fmap (>>= fromEntityProperties) $
  liftF $ GetEntityById (nameOfEntity proxy) ident id

deleteEntityById :: IsEntity e => Proxy e -> Int -> Effect ()
deleteEntityById proxy ident =
  liftF $ DeleteEntityById (nameOfEntity proxy) ident ()

updateEntityById :: IsEntity e => Entity e -> Effect (Maybe (Entity e))
updateEntityById entity =
  fmap (>>= fromEntityProperties) $
  liftF $ UpdateEntityById (toProperties <$> entity) id

selectEntities :: IsEntity e => Proxy e -> Selector -> Effect [Entity e]
selectEntities proxy selector =
  fmap (>>= fromEntityProperties) $
  liftF $ SelectEntities (nameOfEntity proxy) selector id

deleteEntities :: IsEntity e => Proxy e -> Selector -> Effect Int
deleteEntities proxy selector =
  liftF $ DeleteEntities (nameOfEntity proxy) selector id

now :: Effect UTCTime
now = liftF $ Now id

httpRequest :: Request -> Effect (Response B8.ByteString)
httpRequest request = liftF $ HttpRequest request id

twitchApiRequest :: Request -> Effect (Response B8.ByteString)
twitchApiRequest request = liftF $ TwitchApiRequest request id

githubApiRequest :: Request -> Effect (Response B8.ByteString)
githubApiRequest request = liftF $ GitHubApiRequest request id

timeout :: Integer -> Maybe Channel -> Effect () -> Effect ()
timeout t c e = liftF $ TimeoutEff t c e ()

errorEff :: T.Text -> Effect a
errorEff t = liftF $ ErrorEff t

listen :: Effect () -> Effect [T.Text]
listen effect = liftF $ Listen effect id

periodicEffect :: Integer -> Maybe Channel -> Effect () -> Effect ()
periodicEffect period channel effect = do
  effect
  timeout period channel $ periodicEffect period channel effect

periodicEffect' :: Maybe Channel -> Effect (Maybe Integer) -> Effect ()
periodicEffect' channel effect = do
  period' <- effect
  maybe
    (return ())
    (\period -> timeout period channel $ periodicEffect' channel effect)
    period'

twitchCommand :: Channel -> T.Text -> [T.Text] -> Effect ()
twitchCommand channel name args = liftF $ TwitchCommand channel name args ()

randomMarkov :: Effect (Maybe T.Text)
randomMarkov = liftF $ RandomMarkov id

reloadMarkov :: Effect (Maybe T.Text)
reloadMarkov = liftF $ ReloadMarkov id

callFun :: T.Text -> [T.Text] -> Effect (Maybe T.Text)
callFun name args = liftF $ CallFun name args id

randomEff :: (Int, Int) -> Effect Int
randomEff range = liftF $ RandomEff range id
