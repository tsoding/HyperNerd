{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Links
  ( forbidLinksForPlebs
  , textContainsLink
  , trustCommand
  , untrustCommand
  , amitrustedCommand
  , istrustedCommand
  , findTrustedSender
  ) where

import Bot.Replies
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Effect
import Entity
import HyperNerd.Comonad
import HyperNerd.Functor
import Property
import Reaction
import Text.InterpolatedString.QM
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import Transport

-- TODO(#264): trusted users system doesn't handle name changes
newtype TrustedUser = TrustedUser
  { trustedUserName :: T.Text
  }

instance IsEntity TrustedUser where
  nameOfEntity _ = "TrustedUser"
  toProperties trustedUser =
    M.fromList
      [ ( "user"
        , PropertyText $ T.strip $ T.toLower $ trustedUserName trustedUser)
      ]
  fromProperties properties =
    TrustedUser . T.strip . T.toLower <$> extractProperty "user" properties

findTrustedUser :: T.Text -> MaybeT Effect (Entity TrustedUser)
findTrustedUser name =
  MaybeT $
  fmap listToMaybe $
  selectEntities Proxy $ Filter (PropertyEquals "user" $ PropertyText name) All

findTrustedSender :: Sender -> MaybeT Effect (Entity TrustedUser)
findTrustedSender = findTrustedUser . senderName

autoTrustSender :: Sender -> MaybeT Effect (Entity TrustedUser)
autoTrustSender sender
  | senderSubscriber sender || senderAuthority sender =
    MaybeT $ fmap Just $ createEntity Proxy $ TrustedUser $ senderName sender
  | otherwise = MaybeT $ return Nothing

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

-- TODO: forbidLinksForPlebs works only in Twitch
forbidLinksForPlebs :: Message T.Text -> Effect Bool
forbidLinksForPlebs (Message { messageSender = sender@Sender {senderChannel = TwitchChannel _}
                             , messageContent = text
                             })
  | textContainsLink text = do
    trustedUser <-
      runMaybeT (findTrustedSender sender <|> autoTrustSender sender)
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

trustCommand :: Reaction Message T.Text
trustCommand =
  Reaction $ \Message {messageSender = sender, messageContent = inputUser} -> do
    let user = T.strip $ T.toLower inputUser
    trustedUser <- runMaybeT $ findTrustedUser user
    case trustedUser of
      Just _ -> replyToSender sender [qm|{user} is already trusted|]
      Nothing -> do
        void $ createEntity Proxy $ TrustedUser user
        replyToSender sender [qm|{user} is now trusted|]

untrustCommand :: Reaction Message T.Text
untrustCommand =
  Reaction $ \Message {messageSender = sender, messageContent = inputUser} -> do
    let user = T.strip $ T.toLower inputUser
    trustedUser <- runMaybeT $ findTrustedUser user
    case trustedUser of
      Just trustedUser' -> do
        deleteEntityById (Proxy :: Proxy TrustedUser) $ entityId trustedUser'
        replyToSender sender [qm|{user} is not trusted anymore|]
      Nothing ->
        replyToSender sender [qm|{user} was not trusted in the first place|]

amitrustedCommand :: Reaction Message ()
amitrustedCommand =
  cmapR (const id) $
  transR (reflect messageSender) $
  liftR
    (\sender -> runMaybeT (findTrustedSender sender <|> autoTrustSender sender)) $
  cmapR (maybe "no PepeHands" (const "yes Pog")) $ Reaction replyMessage

istrustedCommand :: Reaction Message T.Text
istrustedCommand =
  cmapR (T.strip . T.toLower) $
  cmapR (join (,)) $
  transR ComposeCC $
  liftR (runMaybeT . findTrustedUser) $
  cmapR (maybe " is not trusted PepeHands" (const " is trusted Pog")) $
  transR getComposeCC $ cmapR (uncurry T.append) $ Reaction replyMessage
