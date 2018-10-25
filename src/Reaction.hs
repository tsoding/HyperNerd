module Reaction where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Compose
import Effect
import Events

newtype Reaction a = Reaction { runReaction :: a -> Effect () }

instance Contravariant Reaction where
    contramap f reaction = Reaction $ runReaction reaction . f

type MsgReaction a = ComposeCF Reaction Message a

msgReaction :: (Message a -> Effect ()) -> MsgReaction a
msgReaction = ComposeCF . Reaction

runMsgReaction :: MsgReaction a -> Message a -> Effect ()
runMsgReaction = runReaction . getComposeCF
