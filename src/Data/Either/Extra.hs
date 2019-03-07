module Data.Either.Extra where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just right) = Right right

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x
