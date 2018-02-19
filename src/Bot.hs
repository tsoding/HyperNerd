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

commands :: M.Map T.Text (T.Text -> T.Text -> Effect ())
commands = M.fromList [ ("russify", russifyCommand)
                      , ("addquote", addQuoteCommand)
                      , ("quote", quoteCommand)
                      ]

bot :: Bot
bot Join = say $ T.pack "HyperNyard"
bot (Msg user text) = maybe (return ())
                            (dispatchCommand user)
                            (textAsCommand text)

dispatchCommand :: T.Text -> Command T.Text -> Effect ()
dispatchCommand user command =
    maybe (return ())
          (\f -> f user $ commandArgs command)
          (M.lookup (commandName command) commands)

russifyCommand :: T.Text -> T.Text -> Effect ()
russifyCommand sender westernSpyMsg =
    replyToUser sender $ russify westernSpyMsg

addQuoteCommand :: T.Text -> T.Text -> Effect ()
addQuoteCommand sender quoteContent =
    (quoteProperties quoteContent sender <$> now)
    >>= createEntity "quote"
    >>= (quoteAddedReply sender . entityId)

quoteCommand :: T.Text -> T.Text -> Effect ()
quoteCommand sender "" =
    do quoteEntity <- getRandomEntity "quote"
       quoteFoundReply sender quoteEntity
quoteCommand sender quoteIdText =
    case readMaybe $ T.unpack $ quoteIdText of
      Nothing -> replyToUser sender "Couldn't find any quotes"
      Just quoteId -> do quoteEntity <- getEntityById "quote" quoteId
                         quoteFoundReply sender quoteEntity

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.pack $ printf "@%s %s" user text

quoteAddedReply :: T.Text -> Int -> Effect ()
quoteAddedReply user quoteId =
    replyToUser user $ T.pack $ printf "Added the quote under the number %d" quoteId

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
