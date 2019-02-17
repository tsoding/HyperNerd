module Data.Either.Extra where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just right) = Right right
