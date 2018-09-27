{-# LANGUAGE OverloadedStrings #-}
module Bot.Links (forbidLinksForPlebs, textContainsLink) where

import           Bot.Replies
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf
import           Text.Regex

newtype TrustedUser = TrustedUser { trustedUserName :: T.Text }

instance IsEntity TrustedUser where
    toProperties trustedUser =
        M.fromList [ ("user", PropertyText $ trustedUserName trustedUser) ]
    fromProperties properties =
        do user <- extractProperty "user" properties
           return $ TrustedUser user

findTrustedUser :: T.Text -> Effect (Maybe (Entity TrustedUser))
findTrustedUser name =
    fmap listToMaybe $
    selectEntities "TrustedUser" $
    Filter (PropertyEquals "user" $ PropertyText name) $
    All

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

-- TODO: trust command
-- TODO: untrust command
-- TODO: amitrusted command
