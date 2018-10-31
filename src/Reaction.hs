module Reaction where

import Control.Monad
import Effect
import Events

newtype Reaction a = Reaction
  { runReaction :: a -> Effect ()
  }

type ReactionM a = Reaction (Message a)

cmap :: (a -> b) -> Reaction b -> Reaction a
cmap f reaction = Reaction (runReaction reaction . f)

cmapF :: Functor f => (a -> b) -> Reaction (f b) -> Reaction (f a)
cmapF f reaction = Reaction (runReaction reaction . fmap f)

liftK :: (a -> Effect b) -> Reaction b -> Reaction a
liftK f reaction = Reaction (f >=> runReaction reaction)

liftKM :: (a -> Effect b) -> ReactionM b -> ReactionM a
liftKM f reaction =
  Reaction $ \msg ->
    fmap (\x -> const x <$> msg) (f $ messageContent msg) >>=
    runReaction reaction

liftE :: Effect a -> Reaction a -> Reaction b
liftE = liftK . const

liftEM :: Effect a -> ReactionM a -> ReactionM b
liftEM = liftKM . const

ignore :: Reaction a
ignore = Reaction (const $ return ())

ignoreNothing :: Reaction a -> Reaction (Maybe a)
ignoreNothing = Reaction . maybe (return ()) . runReaction
