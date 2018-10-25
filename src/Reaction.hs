module Reaction where

import Effect
import Events

newtype Reaction a = Reaction { runReaction :: a -> Effect () }

type MsgReaction a = Reaction (Message a)
