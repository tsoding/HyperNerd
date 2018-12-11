{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Links
  ( forbidLinksForPlebs
  , textContainsLink
  , trustCommand
  , untrustCommand
  , amitrustedCommand
  , istrustedCommand
  ) where

import Bot.Replies
import Command
import Control.Monad
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Effect
import Entity
import Events
import HyperNerd.Comonad
import HyperNerd.Functor
import Property
import Reaction
import Text.InterpolatedString.QM
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

-- TODO(#264): trusted users system doesn't handle name changes
newtype TrustedUser = TrustedUser
  { trustedUserName :: T.Text
  }

instance IsEntity TrustedUser where
  toProperties trustedUser =
    M.fromList
      [ ( "user"
        , PropertyText $ T.strip $ T.toLower $ trustedUserName trustedUser)
      ]
  fromProperties properties =
    TrustedUser . T.strip . T.toLower <$> extractProperty "user" properties

findTrustedUser :: T.Text -> Effect (Maybe (Entity TrustedUser))
findTrustedUser name =
  fmap listToMaybe $
  selectEntities "TrustedUser" $
  Filter (PropertyEquals "user" $ PropertyText name) All

textContainsLink :: T.Text -> Bool
textContainsLink t =
  isRight $ do
    regex <-
      compile
        defaultCompOpt
        defaultExecOpt
        "[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&\\/\\/=]*)"
    match <- execute regex $ T.unpack t
    case match of
      Just x -> Right x
      Nothing -> Left "No match found"

forbidLinksForPlebs :: Event -> Effect Bool
forbidLinksForPlebs (Msg sender text)
  | textContainsLink text = do
    trustedUser <- findTrustedUser $ senderName sender
    case trustedUser of
      Nothing
        | not (senderSubscriber sender) && not (senderAuthority sender) -> do
          timeoutSender 1 sender
          whisperToSender
            sender
            [qms|You have been timed out because
                 I thought you sent a link. Only
                 trusted users are allowed
                 to send links. Sometimes I get
                 things wrong. In that case feel
                 free to file an issue at
                 https://github.com/tsoding/HyperNerd/issues .
                 Ask Tsoding to make you a trusted user.|]
          replyToSender sender "check your whispers."
          return True
      _ -> return False
  | otherwise = return False
forbidLinksForPlebs _ = return False

trustCommand :: CommandHandler T.Text
trustCommand Message {messageSender = sender, messageContent = inputUser} = do
  let user = T.strip $ T.toLower inputUser
  trustedUser <- findTrustedUser user
  case trustedUser of
    Just _ -> replyToSender sender [qm|{user} is already trusted|]
    Nothing -> do
      void $ createEntity "TrustedUser" $ TrustedUser user
      replyToSender sender [qm|{user} is now trusted|]

untrustCommand :: CommandHandler T.Text
untrustCommand Message {messageSender = sender, messageContent = inputUser} = do
  let user = T.strip $ T.toLower inputUser
  trustedUser <- findTrustedUser user
  case trustedUser of
    Just trustedUser' -> do
      deleteEntityById "TrustedUser" $ entityId trustedUser'
      replyToSender sender [qm|{user} is not trusted anymore|]
    Nothing ->
      replyToSender sender [qm|{user} was not trusted in the first place|]

amitrustedCommand :: Reaction Message ()
amitrustedCommand =
  cmapR (const id) $
  transR (reflect (senderName . messageSender)) $
  liftR findTrustedUser $
  cmapR (maybe "no PepeHands" (const "yes Pog")) $ Reaction replyMessage

istrustedCommand :: Reaction Message T.Text
istrustedCommand =
  cmapR (T.strip . T.toLower) $
  cmapR (join (,)) $
  transR ComposeCC $
  liftR findTrustedUser $
  cmapR (maybe " is not trusted PepeHands" (const " is trusted Pog")) $
  transR getComposeCC $ cmapR (uncurry T.append) $ Reaction replyMessage
