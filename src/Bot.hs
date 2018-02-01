module Bot (Bot, bot, Event(..), Effect(..)) where

import Command
import Data.Maybe
import qualified Data.Text as T
import Russify

type Bot s = Event -> Effect s

data Event = Join
           | Msg T.Text T.Text

data Effect s = None
              | Say T.Text

bot :: Bot s
bot Join = Say $ T.pack "HyperNyard"
bot (Msg user text) = maybe None (effectOfCommand user) $ textAsCommand text

effectOfCommand :: T.Text -> Command T.Text -> Effect s
effectOfCommand sender command =
    case T.unpack $ commandName command of
      "russify" -> replyToUser sender
                   $ russify
                   $ commandArgs command
      _ -> None

replyToUser :: T.Text -> T.Text -> Effect s
replyToUser user text = Say $ T.concat [ (T.pack "@")
                                       , user
                                       , (T.pack " ")
                                       , text]
