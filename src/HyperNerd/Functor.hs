module HyperNerd.Functor where

reflect :: Functor f => (f (a -> b) -> a) -> f (a -> b) -> f b
reflect r f = ($ r f) <$> f
