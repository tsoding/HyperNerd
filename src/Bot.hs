{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import Command
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Russify

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
    (quoteEntity quoteContent sender <$> now)
    >>= saveEntity
    >>= quoteAddedReply sender

-- TODO(#36): implement !quote command

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

quoteEntity :: T.Text -> T.Text -> UTCTime -> Entity
quoteEntity content quoter timestamp =
    Entity { entityName = "quote"
           , entityProperties =
               M.fromList [ ("content", PropertyText content)
                          , ("quoter", PropertyText quoter)
                          , ("timestamp", PropertyUTCTime timestamp)
                          ]
           }
