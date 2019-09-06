{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.CopyPasta
  ( copyPastaFilter
  , countForbiddenCommand
  ) where

import Bot.Replies
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import Reaction
import Text.InterpolatedString.QM
import Transport

countForbidden :: T.Text -> Int
countForbidden = T.length . T.filter (not . isAllowed)

isAllowed :: Char -> Bool
isAllowed = getAny . foldMap (Any .) [isAlpha, isNumber, isSpace, isPunctuation]

copyPastaFilter :: Reaction Message T.Text -> Reaction Message T.Text
copyPastaFilter reaction =
  Reaction $ \case
    Message { messageContent = content
            , messageSender = sender@Sender { senderRoles = roles
                                            , senderChannel = TwitchChannel _
                                            }
            }
      | all (`notElem` permittedRoles) roles && countForbidden content > limit -> do
        timeoutSender penalty sender
        replyToSender
          sender
          [qms|ASCII spam is not allowed anymore.
               Use !asciify command.|]
    msg -> runReaction reaction msg
  where
    limit = 100
    penalty = 30
    permittedRoles = authorityRoles

countForbiddenCommand :: Reaction Message T.Text
countForbiddenCommand = cmapR (T.pack . show . countForbidden) sayMessage
