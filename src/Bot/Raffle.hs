{-# LANGUAGE QuasiQuotes #-}
module Bot.Raffle (raffleCommand, joinCommand) where

import Reaction
import Events
import Data.Time
import Entity
import qualified Data.Map as M
import Property
import Effect
import Text.InterpolatedString.QM
import Control.Comonad

data Raffle = Raffle { raffleStartedAt :: UTCTime
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

-- TODO: currentRaffle is not implemented
currentRaffle :: Effect (Maybe Raffle)
currentRaffle = return Nothing

-- TODO: startNewRaffle is not implemented
startNewRaffle :: Int -> Effect (Entity Raffle)
startNewRaffle _ = undefined

-- TODO: announceRaffleResults is not implemented
announceRaffleResults :: Int -> Effect ()
announceRaffleResults _ = return ()

raffleCommand :: Reaction Message Int
raffleCommand =
  Reaction $ \msg -> do
    let duration = extract msg
    raffle <- currentRaffle
    case raffle of
      Just _ ->
        say
          [qms|Cannot start a new raffle while
               the previous one is still going|]
      Nothing -> do
        raffle' <- startNewRaffle duration
        timeout
          (fromIntegral duration * 1000)
          (announceRaffleResults $ entityId raffle')
        say
          [qms|The raffle has been started. Use !join
                  command to join.|]

-- TODO: implement joinCommand
joinCommand :: Reaction Message a
joinCommand = ignore
