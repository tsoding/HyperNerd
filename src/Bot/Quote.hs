{-# LANGUAGE OverloadedStrings #-}
module Bot.Quote where

import           Bot.Replies
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf
import           Text.Read

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
    fromProperties properties = do
        content   <- extractProperty "content" properties
        quoter    <- extractProperty "quoter" properties
        timestamp <- extractProperty "timestamp" properties
        return Quote { quoteContent = content
                     , quoteQuoter = quoter
                     , quoteTimestamp = timestamp
                     }

addQuoteCommand :: Sender -> T.Text -> Effect ()
addQuoteCommand sender content =
    do timestamp <- now
       quoter    <- return $ senderName sender
       quote     <- return Quote { quoteContent = content
                                 , quoteQuoter = quoter
                                 , quoteTimestamp = timestamp
                                 }
       entity    <- createEntity "quote" quote

       quoteAddedReply quoter $ entityId entity

quoteCommand :: Sender -> T.Text -> Effect ()
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
    replyToUser user
      $ T.pack
      $ printf "Added the quote under the number %d" quoteId

quoteFoundReply :: T.Text -> Maybe (Entity Properties) -> Effect ()
quoteFoundReply user Nothing = replyToUser user "Couldn't find any quotes"
quoteFoundReply user (Just entity) =
    do quote <- fromEntityProperties entity
       replyToUser user $ T.pack $ printf "%s (%d)" (quoteContent $ entityPayload quote) (entityId entity)
