{-# LANGUAGE DeriveFunctor #-}

module HyperNerd.Comonad where

import Control.Comonad

newtype ComposeCC f g a = ComposeCC
  { getComposeCC :: f (g a)
  } deriving (Functor)

instance (Comonad f, Comonad g) => Comonad (ComposeCC f g) where
  extract = extract . extract . getComposeCC
  duplicate c = c <$ c
