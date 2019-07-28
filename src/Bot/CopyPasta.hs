{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.CopyPasta where

import Bot.Replies
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import Reaction
import Text.InterpolatedString.QM
import Transport

copyPastaFilter :: Reaction Message T.Text -> Reaction Message T.Text
copyPastaFilter reaction =
  Reaction $ \case
    Message { messageContent = content
            , messageSender = sender@Sender { senderRoles = []
                                            , senderChannel = TwitchChannel _
                                            }
            }
      | T.length (T.filter (not . isAllowed) content) > limit -> do
        timeoutSender penalty sender
        replyToSender
          sender
          [qms|Spam is allowed only to trusted users.
               Subscribe to gain trust instantly:
               https://www.twitch.tv/products/tsoding|]
    msg -> runReaction reaction msg
  where
    limit = 100
    penalty = 300
    isAllowed =
      getAny . foldMap (Any .) [isAlphaNum, isMark, isSpace, isPunctuation]
