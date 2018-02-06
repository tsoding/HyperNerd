module Effect ( Effect
              , EffectF (..)
              , ok
              , reportError
              , say
              ) where

import Control.Monad.Free
import qualified Data.Text as T

data EffectF s = Ok
               | ReportError T.Text T.Text
               | Say T.Text s

instance Functor EffectF where
    fmap f (Ok)            = Ok
    fmap f (ReportError logText userResponse) =
        ReportError logText userResponse
    fmap f (Say msg s)      = Say msg (f s)

type Effect = Free EffectF

ok :: Effect ()
ok = liftF Ok

reportError :: T.Text -> T.Text -> Effect ()
reportError logText userResponse =
    liftF $ ReportError logText userResponse

say :: T.Text -> Effect ()
say msg = liftF $ Say msg ()
