{-# LANGUAGE DeriveFunctor #-}

module Free
  ( Free(..)
  , liftF
  ) where

liftF :: Functor f => f a -> Free f a
liftF f = Free (Pure <$> f)

data Free f a
  = Pure a
  | Free (f (Free f a))
  deriving (Functor)

instance Functor f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  (Pure f) <*> (Pure x) = pure $ f x
  (Pure f) <*> (Free wwx) = Free ((f <$>) <$> wwx)
  (Free wwf) <*> ax = Free ((<*> ax) <$> wwf)

instance Functor f => Monad (Free f) where
  (Pure x) >>= f = f x
  (Free wwx) >>= f = Free ((>>= f) <$> wwx)
