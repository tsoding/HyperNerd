{-# LANGUAGE DeriveFunctor #-}

module HyperNerd.Comonad where

import Data.Functor.Compose

class Functor w => Comonad w where
    duplicate :: w a -> w (w a)
    extract :: w a -> a

instance (Comonad f, Comonad g) => Comonad (Compose f g) where
  extract = extract . extract . getCompose
  duplicate c = c <$ c

instance Comonad ((,) a) where
  extract = snd
  {-# INLINE extract #-}
  duplicate p = (fst p, p)
  {-# INLINE duplicate #-}
