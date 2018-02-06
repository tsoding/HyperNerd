{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import Command
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Effect
import Russify
import Entity

data Event = Join
           | Msg T.Text T.Text

type Bot = Event -> Effect ()

bot :: Bot
bot Join = say $ T.pack "HyperNyard"
bot (Msg user text) = maybe ok (effectOfCommand user) $ textAsCommand text

effectOfCommand :: T.Text -> Command T.Text -> Effect ()
effectOfCommand sender command =
    -- TODO(#35): refactor out case in effectOfCommand
    -- Use function level pattern matching
    case T.unpack $ commandName command of
      "russify" -> replyToUser sender
                   $ russify
                   $ commandArgs command
      "addquote" -> do timestamp <- now
                       let entity =
                               Entity { entityName = "quote"
                                      , entityProperties =
                                          M.fromList [ ("content", PropertyText $ commandArgs command)
                                                     , ("quoter", PropertyText $ sender)
                                                     , ("timestamp", PropertyUTCTime $ timestamp)
                                                     ]
                                      }
                       entityId <- saveEntity entity
                       replyToUser sender $ T.concat [ "Added the quote under the number "
                                                     , T.pack $ show entityId
                                                     ]
      -- TODO: implement !quote command
      _ -> ok

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.concat [ (T.pack "@")
                                       , user
                                       , (T.pack " ")
                                       , text]
