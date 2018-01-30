module Bot (Bot, bot, Event(..), Effect(..)) where

import Command
import Data.Maybe
import qualified Data.Text as T
import Russify

type Bot s = Event -> Effect s

data Event = Join
           | Msg T.Text

data Effect s = None
              | Say T.Text

bot :: Bot s
bot Join = Say $ T.pack "HyperNyard"
bot (Msg text) = maybe None effectOfCommand $ textAsCommand text

effectOfCommand :: Command T.Text -> Effect s
effectOfCommand command =
    case T.unpack $ commandName command of
      -- TODO(#23): !russify command should mention the command invoker
      "russify" -> Say $ russify $ commandArgs command
      _ -> None
