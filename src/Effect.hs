module Effect ( Effect
              , EffectF (..)
              , ok
              , say
              ) where

import Control.Monad.Free
import qualified Data.Text as T

data EffectF s = Ok s
               | Say T.Text s

instance Functor EffectF where
    fmap f (Ok s)           = Ok (f s)
    fmap f (Say msg s)      = Say msg (f s)

type Effect = Free EffectF

ok :: Effect ()
ok = liftF $ Ok ()

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()
