{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Asciify
  ( asciifyReaction
  ) where

import Bot.BttvFfz
import Bot.Replies
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Data.Time.Extra
import Effect
import Entity
import Louis
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport

newtype AsciifyState = AsciifyState
  { asciifyStateLastUsed :: UTCTime
  }

instance IsEntity AsciifyState where
  nameOfEntity Proxy = "AsciifyState"
  toProperties entity =
    M.fromList [("lastUsed", PropertyUTCTime $ asciifyStateLastUsed entity)]
  fromProperties properties =
    AsciifyState <$> extractProperty "lastUsed" properties

currentAsciifyState :: Effect (Entity AsciifyState)
currentAsciifyState = do
  maybeState <- listToMaybe <$> selectEntities Proxy All
  case maybeState of
    Just state -> return state
    Nothing -> createEntity Proxy =<< AsciifyState <$> now

asciifyCooldown :: Reaction Message a -> Reaction Message a
asciifyCooldown next =
  Reaction $ \msg -> do
    state <- currentAsciifyState
    currentTime <- now
    let diff =
          diffUTCTime currentTime $ asciifyStateLastUsed $ entityPayload state
    let cooldown = 2 * 60
    if diff > cooldown
      then do
        void $ updateEntityById $ AsciifyState currentTime <$ state
        runReaction next msg
      else replyToSender
             (messageSender msg)
             [qms|Command has not cooled down yet:
                  {humanReadableDiffTime (cooldown - diff)} left.|]

asciifyReaction :: Reaction Message T.Text
asciifyReaction =
  liftR
    (\name -> do
       ffz <- ffzUrlByName name
       bttv <- bttvUrlByName name
       return (ffz <|> bttv)) $
  replyOnNothing "Such emote does not exist" $
  asciifyCooldown $
  byteStringHttpRequestReaction $
  cmapR (braillizeByteString . BSL.toStrict) $
  eitherReaction (Reaction (logMsg . T.pack . messageContent)) $
  dupCmapR
    (\Message { messageSender = Sender {senderChannel = channel}
              , messageContent = msg
              } ->
       case channel of
         TwitchChannel _ -> T.unwords msg
         DiscordChannel _ -> T.unlines msg)
    sayMessage
