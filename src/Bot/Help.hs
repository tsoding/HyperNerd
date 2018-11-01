{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Help
  ( helpCommand
  ) where

import Text.InterpolatedString.QM
import Command
import Reaction
import Events
import qualified Data.Text as T
import qualified Data.Map as M
import Bot.Replies
import Data.List

helpCommand :: CommandTable -> Reaction Message T.Text
helpCommand commandTable =
  ifR
    T.null
    (replyAvaliableCommands commandTable)
    (replyHelpForCommand commandTable)

replyHelpForCommand :: CommandTable -> Reaction Message T.Text
replyHelpForCommand commandTable =
  cmapR (`M.lookup` commandTable) $
  replyOnNothing "Cannot find such command FeelsBadMan" $
  cmapR fst $ Reaction replyMessage

replyAvaliableCommands :: CommandTable -> Reaction Message T.Text
replyAvaliableCommands commandTable =
  cmapR (const $ availableCommandsReply commandTable) $ Reaction replyMessage

availableCommandsReply :: CommandTable -> T.Text
availableCommandsReply commandTable =
  let commandList =
        T.concat $
        intersperse (T.pack ", ") $
        map (\x -> T.concat [T.pack "!", x]) $ M.keys commandTable
   in [qm|Available commands: {commandList}|]
