module Data.Maybe.Extra where

toMaybe :: a -> Bool -> Maybe a
toMaybe x True = Just x
toMaybe _ False = Nothing

maybePredicate :: (a -> Bool) -> a -> Maybe a
maybePredicate p x = toMaybe x (p x)
