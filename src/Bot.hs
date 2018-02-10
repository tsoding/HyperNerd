{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import           Command
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Russify

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

effectOfCommand _ _ = return ()

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.concat [ (T.pack "@")
                                       , user
                                       , (T.pack " ")
                                       , text]

quoteAddedReply :: T.Text -> Int -> Effect ()
quoteAddedReply user quoteId =
    replyToUser user $ T.concat [ "Added the quote under the number "
                                , T.pack $ show quoteId
                                ]

-- TODO: Bot.quoteFoundReply is too messy
quoteFoundReply :: T.Text -> Maybe Entity -> Effect ()
quoteFoundReply user (Nothing) = replyToUser user "Couldn't find any quotes"
quoteFoundReply user (Just entity) =
    case M.lookup "content" $ entityProperties entity of
      Nothing ->
          do logMsg $ T.concat [ "Quote #"
                               , T.pack $ show $ entityId entity
                               , " doesn't have the 'content field'"
                               ]
             replyToUser user "Couldn't find any quotes, because of some database issues."
      Just (PropertyText content) ->
          replyToUser user $ T.concat [ content, " (", T.pack $ show $ entityId entity, ")" ]
      Just _ -> do logMsg $ T.concat [ "Quote #"
                                     , T.pack $ show $ entityId entity
                                     , " content is not text"
                                     ]
                   replyToUser user "Couldn't find any quotes, because of some database issues."


quoteProperties :: T.Text -> T.Text -> UTCTime -> Properties
quoteProperties content quoter timestamp =
    M.fromList [ ("content", PropertyText content)
               , ("quoter", PropertyText quoter)
               , ("timestamp", PropertyUTCTime timestamp)
               ]
