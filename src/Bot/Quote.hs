{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Quote where

import           Bot.Replies
import           Command
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
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

deleteQuoteCommand :: CommandHandler Int
deleteQuoteCommand Message { messageSender = sender
                           , messageContent = quoteId
                           } = do
  deleteEntityById "quote" quoteId
  replyToSender sender "Quote has been deleted"

addQuoteCommand :: CommandHandler T.Text
addQuoteCommand Message { messageSender = sender
                        , messageContent = content
                        } = do
  timestamp <- now
  entity <- createEntity "quote" Quote { quoteContent = content
                                       , quoteQuoter = senderName sender
                                       , quoteTimestamp = timestamp
                                       }
  replyToSender sender [qms|Added the quote under the
                            number {entityId entity}|]

quoteCommand :: CommandHandler (Maybe Int)
quoteCommand Message { messageSender = sender
                     , messageContent = Nothing
                     } =
    fmap listToMaybe (selectEntities "quote" (Take 1 $ Shuffle All))
      >>= quoteFoundReply sender
quoteCommand Message { messageSender = sender
                     , messageContent = Just quoteId
                     } = do
  quote <- getEntityById "quote" quoteId
  quoteFoundReply sender quote

quoteFoundReply :: Sender -> Maybe (Entity Quote) -> Effect ()
quoteFoundReply sender Nothing =
    replyToSender sender "Couldn't find any quotes"
quoteFoundReply sender (Just Entity { entityId = quoteId
                                    , entityPayload =
                                        Quote { quoteContent = content }
                                    }) =
    replyToSender sender [qms|{content} {quoteId}|]
