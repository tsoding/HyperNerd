{-# LANGUAGE QuasiQuotes #-}

module Bot.Raffle
  ( raffleCommand
  , joinCommand
  ) where

import Bot.Replies
import qualified Data.Map as M
import Data.Time
import Effect
import Entity
import Events
import HyperNerd.Functor
import Property
import Reaction
import Text.InterpolatedString.QM

data Raffle = Raffle
  { raffleStartedAt :: UTCTime
  , raffleDuration :: Int
  }

instance IsEntity Raffle where
  toProperties raffle =
    M.fromList
      [ ("startedAt", PropertyUTCTime $ raffleStartedAt raffle)
      , ("duration", PropertyInt $ raffleDuration raffle)
      ]
  fromProperties properties =
    Raffle <$> extractProperty "startedAt" properties <*>
    extractProperty "duration" properties

-- TODO(#442): currentRaffle is not implemented
currentRaffle :: Effect (Maybe (Entity Raffle))
currentRaffle = return Nothing

-- TODO(#443): startNewRaffle is not implemented
startNewRaffle :: Int -> Effect (Maybe (Entity Raffle))
startNewRaffle _ = return Nothing

-- TODO(#444): joinUser is not implemented
joinUser :: Entity Raffle -> Sender -> Effect ()
joinUser _ _ = return ()

raffleCommand :: Reaction Message Int
raffleCommand =
  liftR startNewRaffle $
  maybeReaction
    (cmapR
       (const
          [qms|Cannot start a new raffle while
               the previous one is still going|]) $
     Reaction replyMessage)
    (Reaction $
     const $
     say
       [qms|The raffle has been started. Use !join
            command to join.)|])

joinCommand :: Reaction Message a
joinCommand =
  liftR (const currentRaffle) $
  ignoreNothing $ cmapR joinUser $ transR (reflect messageSender) ignore
