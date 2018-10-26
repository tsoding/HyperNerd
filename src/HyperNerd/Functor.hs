module HyperNerd.Functor where

outerProduct :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
outerProduct f fa gb = (<$> gb) . f <$> fa

outerProduct' :: (Functor f, Functor g) => (a -> b -> c) -> g b -> f a -> f (g c)
outerProduct' = flip . outerProduct

reflect :: Functor f => (f (a -> b) -> a) -> f (a -> b) -> f b
reflect r f = ($ (r f)) <$> f
