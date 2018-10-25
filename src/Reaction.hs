module Reaction where

import Effect
import Events
import Control.Monad

newtype Reaction a = Reaction { runReaction :: a -> Effect () }

type MsgReaction a = Reaction (Message a)

cmap :: (a -> b) -> Reaction b -> Reaction a
cmap f reaction = Reaction (runReaction reaction . f)

cmapF :: Functor f => (a -> b) -> Reaction (f b) -> Reaction (f a)
cmapF f reaction = Reaction (runReaction reaction . fmap f)

liftE :: (a -> Effect b) -> Reaction b -> Reaction a
liftE f reaction = Reaction $ (f >=> runReaction reaction)
