module Data.Maybe.Extra where

toMaybe :: a -> Bool -> Maybe a
toMaybe x True = Just x
toMaybe _ False = Nothing
