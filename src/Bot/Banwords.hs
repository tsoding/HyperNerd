{-# LANGUAGE OverloadedStrings #-}

module Bot.Banwords where

import Bot.Replies
import qualified Data.Text as T
import Effect
import Events

forbidBanwords :: Message T.Text -> Effect Bool
forbidBanwords Message {messageContent = text, messageSender = sender} =
  if "theart3Screw" `T.isInfixOf` text
    then do
      timeoutSender 600 sender
      replyToSender sender "Screwing request accepted Jebaited"
      return True
    else return False
