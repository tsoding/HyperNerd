module Data.Maybe.Extra where

import qualified Control.Monad.Fail as Fail

toMaybe :: a -> Bool -> Maybe a
toMaybe x True = Just x
toMaybe _ False = Nothing

maybePredicate :: (a -> Bool) -> a -> Maybe a
maybePredicate p x = toMaybe x (p x)

maybeFail :: Fail.MonadFail m => String -> Maybe a -> m a
maybeFail _ (Just a) = return a
maybeFail msg Nothing = Fail.fail msg
