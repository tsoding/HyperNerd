{-# LANGUAGE OverloadedStrings #-}
module Bot.Links (forbidLinksForPlebs) where

import           Bot.Replies
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Events
import           Text.Printf
import           Text.Regex

textContainsLink :: T.Text -> Bool
textContainsLink t = isJust
                       $ matchRegex (mkRegex "[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&\\/\\/=]*)")
                       $ T.unpack t

senderIsPleb :: Sender -> Bool
senderIsPleb sender = not (senderSubscriber sender) && not (senderMod sender)

forbidLinksForPlebs :: Event -> Maybe (Effect())
forbidLinksForPlebs (Msg sender text)
    | textContainsLink text && senderIsPleb sender =
        return $ do say $ T.pack $ printf "/timeout %s 1" $ senderName sender
                    replyToUser (senderName sender)
                                "Only subs can post links, sorry."
    | otherwise = Nothing
forbidLinksForPlebs _ = Nothing
