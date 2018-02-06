module Bot (Bot, bot, Event(..)) where

import Command
import Data.Maybe
import qualified Data.Text as T
import Effect
import Russify

data Event = Join
           | Msg T.Text T.Text

type Bot = Event -> Effect ()

bot :: Bot
bot Join = say $ T.pack "HyperNyard"
bot (Msg user text) = maybe ok (effectOfCommand user) $ textAsCommand text

effectOfCommand :: T.Text -> Command T.Text -> Effect ()
effectOfCommand sender command =
    case T.unpack $ commandName command of
      "russify" -> replyToUser sender
                   $ russify
                   $ commandArgs command
      _ -> ok

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.concat [ (T.pack "@")
                                       , user
                                       , (T.pack " ")
                                       , text]
