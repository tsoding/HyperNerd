{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Links ( forbidLinksForPlebs
                 , textContainsLink
                 , trustCommand
                 , untrustCommand
                 , amitrustedCommand
                 , istrustedCommand
                 ) where

import           Bot.Replies
import           Command
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Property
import           Text.InterpolatedString.QM
import           Text.Regex

-- TODO(#264): trusted users system doesn't handle name changes
newtype TrustedUser = TrustedUser { trustedUserName :: T.Text }

instance IsEntity TrustedUser where
    toProperties trustedUser =
        M.fromList [ ("user", PropertyText $ trustedUserName trustedUser) ]
    fromProperties properties =
        TrustedUser <$> extractProperty "user" properties

findTrustedUser :: T.Text -> Effect (Maybe (Entity TrustedUser))
findTrustedUser name =
    fmap listToMaybe $
    selectEntities "TrustedUser" $
    Filter (PropertyEquals "user" $ PropertyText name) All

textContainsLink :: T.Text -> Bool
textContainsLink t = isJust
                       $ matchRegex (mkRegex "[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&\\/\\/=]*)")
                       $ T.unpack t

forbidLinksForPlebs :: Event -> Effect Bool
forbidLinksForPlebs (Msg sender text)
    | textContainsLink text = do
        trustedUser <- findTrustedUser $ senderName sender
        case trustedUser of
          Just _ -> return False
          Nothing -> do timeoutSender 30 sender
                        whisperToSender sender [qms|You have been timed out because
                                                    I thought you sent a link. Only
                                                    trusted users are allowed to send links.
                                                    Sometimes I get things wrong. In that case feel
                                                    free to file an issue at https://github.com/tsoding/HyperNerd/issues.
                                                    Ask Tsoding to make you a trusted user.|]
                        replyToSender sender "check your whispers"
                        return True
    | otherwise = return False
forbidLinksForPlebs _ = return False

trustCommand :: CommandHandler T.Text
trustCommand sender inputUser = do
  let user = T.toLower inputUser
  trustedUser <- findTrustedUser user
  case trustedUser of
    Just _  -> replyToSender sender [qm|{user} is already trusted|]
    Nothing -> do _ <- createEntity "TrustedUser" $ TrustedUser user
                  replyToSender sender [qm|{user} is now trusted|]

untrustCommand :: CommandHandler T.Text
untrustCommand sender inputUser = do
  let user = T.toLower inputUser
  trustedUser <- findTrustedUser user
  case trustedUser of
    Just trustedUser' -> do deleteEntityById "TrustedUser" $ entityId trustedUser'
                            replyToSender sender [qm|{user} is not trusted anymore|]
    Nothing -> replyToSender sender [qm|{user} was not trusted in the first place|]

amitrustedCommand :: CommandHandler ()
amitrustedCommand sender () = do
  trustedUser <- findTrustedUser $ senderName sender
  case trustedUser of
    Just _  -> replyToSender sender "Yes Pog"
    Nothing -> replyToSender sender "No PepeHands"

istrustedCommand :: CommandHandler T.Text
istrustedCommand sender inputUser = do
  let user = T.toLower inputUser
  trustedUser <- findTrustedUser user
  case trustedUser of
    Just _ -> replyToSender sender [qm|{user} is trusted Pog|]
    Nothing -> replyToSender sender [qm|{user} is not trusted PepeHands|]
