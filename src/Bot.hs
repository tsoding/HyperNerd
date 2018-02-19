{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import           Command
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Russify
import           Text.Printf
import           Text.Read

data Event = Join
           | Msg T.Text T.Text

type Bot = Event -> Effect ()

bot :: Bot
bot Join = say $ T.pack "HyperNyard"
bot (Msg user text) = maybe (return ()) (effectOfCommand user) $ textAsCommand text

effectOfCommand :: T.Text -> Command T.Text -> Effect ()
effectOfCommand sender (Command { commandName = "russify"
                                , commandArgs = westernSpyMsg }) =
    replyToUser sender $ russify westernSpyMsg
effectOfCommand sender (Command { commandName = "addquote"
                                , commandArgs = quoteContent }) =
    (quoteProperties quoteContent sender <$> now)
    >>= createEntity "quote"
    >>= (quoteAddedReply sender . entityId)
effectOfCommand sender (Command { commandName = "quote"
                                , commandArgs = "" }) =
    do quoteEntity <- getRandomEntity "quote"
       quoteFoundReply sender quoteEntity
effectOfCommand sender (Command { commandName = "quote"
                                , commandArgs = quoteIdText }) =
    case readMaybe $ T.unpack $ quoteIdText of
      Nothing -> replyToUser sender "Couldn't find any quotes"
      Just quoteId -> do quoteEntity <- getEntityById "quote" quoteId
                         quoteFoundReply sender quoteEntity

effectOfCommand _ _ = return ()

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.pack $ printf "@%s %s" user text

quoteAddedReply :: T.Text -> Int -> Effect ()
quoteAddedReply user quoteId =
    replyToUser user $ T.pack $ printf "Added the quote under the number %d" quoteId

-- TODO(#51): Bot.quoteFoundReply is too messy
quoteFoundReply :: T.Text -> Maybe Entity -> Effect ()
quoteFoundReply user (Nothing) = replyToUser user "Couldn't find any quotes"
quoteFoundReply user (Just entity) =
    case M.lookup "content" $ entityProperties entity of
      Nothing ->
          do logMsg $ T.pack $ printf "Quote #%d doesn't have the 'content field'" $ entityId entity
             replyToUser user "Couldn't find any quotes, because of some database issues."
      Just (PropertyText content) ->
          replyToUser user $ T.pack $ printf "%s (%d)" content $ entityId entity
      Just _ -> do logMsg $ T.pack $ printf "Quote #%d content is not text" $ entityId entity
                   replyToUser user "Couldn't find any quotes, because of some database issues."


quoteProperties :: T.Text -> T.Text -> UTCTime -> Properties
quoteProperties content quoter timestamp =
    M.fromList [ ("content", PropertyText content)
               , ("quoter", PropertyText quoter)
               , ("timestamp", PropertyUTCTime timestamp)
               ]
