{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Quote where

import           Bot.Replies
import           Command
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
import           Text.Read
import           Text.InterpolatedString.QM

data Quote = Quote { quoteContent :: T.Text
                   , quoteQuoter :: T.Text
                   , quoteTimestamp :: UTCTime
                   }

instance IsEntity Quote where
    toProperties quote =
        M.fromList [ ("content", PropertyText $ quoteContent quote)
                   , ("quoter", PropertyText $ quoteQuoter quote)
                   , ("timestamp", PropertyUTCTime $ quoteTimestamp quote)
                   ]
    fromProperties properties =
        Quote <$> extractProperty "content" properties
              <*> extractProperty "quoter" properties
              <*> extractProperty "timestamp" properties

deleteQuoteCommand :: CommandHandler T.Text
deleteQuoteCommand sender quoteIdText =
  case readMaybe $ T.unpack quoteIdText of
    Just quoteId -> do deleteEntityById "quote" quoteId
                       replyToSender sender "Quote has been deleted"
    Nothing -> replyToSender sender "Could not find quote with such id"

addQuoteCommand :: CommandHandler T.Text
addQuoteCommand sender content =
    do timestamp <- now
       let quoter = senderName sender
       let quote  = Quote { quoteContent = content
                          , quoteQuoter = quoter
                          , quoteTimestamp = timestamp
                          }
       entity    <- createEntity "quote" quote

       quoteAddedReply quoter $ entityId entity

quoteCommand :: CommandHandler T.Text
quoteCommand sender "" =
    fmap listToMaybe (selectEntities "quote" (Take 1 $ Shuffle All))
      >>= quoteFoundReply (senderName sender)
quoteCommand sender quoteIdText =
    maybe
      (replyToUser (senderName sender) "Couldn't find any quotes")
      (getEntityById "quote" >=> quoteFoundReply (senderName sender))
      (readMaybe $ T.unpack quoteIdText)

quoteAddedReply :: T.Text -> Int -> Effect ()
quoteAddedReply user quoteId =
    replyToUser user [qms|Added the quote under the number {quoteId}|]

quoteFoundReply :: T.Text -> Maybe (Entity Quote) -> Effect ()
quoteFoundReply user Nothing = replyToUser user "Couldn't find any quotes"
quoteFoundReply user (Just entity) = replyToUser user [qms|{content} {quoteId}|]
    where quoteId = entityId entity
          content = quoteContent $ entityPayload entity
