{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.CopyPasta where

import Data.Monoid
import Data.Char
import Reaction
import Transport
import qualified Data.Text as T
import Text.InterpolatedString.QM
import Bot.Replies

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
