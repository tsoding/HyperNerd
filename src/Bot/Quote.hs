{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Quote
  ( deleteQuoteCommand
  , addQuoteCommand
  , quoteCommand
  ) where

import Bot.Replies
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Events
import HyperNerd.Functor
import Property
import Reaction
import Text.InterpolatedString.QM

data Quote = Quote
  { quoteContent :: T.Text
  , quoteQuoter :: T.Text
  , quoteTimestamp :: UTCTime
  }

instance IsEntity Quote where
  toProperties quote =
    M.fromList
      [ ("content", PropertyText $ quoteContent quote)
      , ("quoter", PropertyText $ quoteQuoter quote)
      , ("timestamp", PropertyUTCTime $ quoteTimestamp quote)
      ]
  fromProperties properties =
    Quote <$> extractProperty "content" properties <*>
    extractProperty "quoter" properties <*>
    extractProperty "timestamp" properties

deleteQuoteCommand :: Reaction Message Int
deleteQuoteCommand =
  liftR (deleteEntityById "quote") $
  cmapR (const "Quote has been deleted") $ Reaction replyMessage

addQuoteCommand :: Reaction Message T.Text
addQuoteCommand =
  cmapR Quote $
  transR (reflect (senderName . messageSender)) $
  liftR (<$> now) $
  liftR (createEntity "quote") $
  cmapR
    (\entity ->
       [qms|Added the quote under the
            number {entityId entity}|]) $
  Reaction replyMessage

replyRandomQuote :: Reaction Message ()
replyRandomQuote =
  liftR (const $ selectEntities "quote" $ Take 1 $ Shuffle All) $
  cmapR listToMaybe $ quoteFoundReply

replyRequestedQuote :: Reaction Message Int
replyRequestedQuote = liftR (getEntityById "quote") $ quoteFoundReply

quoteCommand :: Reaction Message (Maybe Int)
quoteCommand = maybeReaction replyRandomQuote replyRequestedQuote

quoteAsReplyMessage :: Entity Quote -> T.Text
quoteAsReplyMessage entity =
  [qms|{quoteContent $ entityPayload entity}
       {entityId entity}|]

quoteFoundReply :: Reaction Message (Maybe (Entity Quote))
quoteFoundReply =
  replyOnNothing "Couldn't find any quotes" $
  cmapR quoteAsReplyMessage $ Reaction replyMessage
