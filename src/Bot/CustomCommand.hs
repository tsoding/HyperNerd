{-# LANGUAGE OverloadedStrings #-}
module Bot.CustomCommand ( addCustomCommand
                         , deleteCustomCommand
                         , dispatchCustomCommand
                         , updateCustomCommand
                         ) where

import           Bot.Replies
import           Bot.Variable
import           Command
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf

data CustomCommand = CustomCommand { customCommandName :: T.Text
                                   , customCommandMessage :: T.Text
                                   , customCommandTimes :: Int
                                   , customCommandLastUsed :: UTCTime
                                   }

instance IsEntity CustomCommand where
    toProperties customCommand =
        M.fromList [ ("name", PropertyText $ customCommandName customCommand)
                   , ("message", PropertyText $ customCommandMessage customCommand)
                   , ("times", PropertyInt $ customCommandTimes customCommand)
                   , ("lastUsed", PropertyUTCTime $ customCommandLastUsed customCommand)
                   ]
    fromProperties properties =
        do name     <- extractProperty "name" properties
           message  <- extractProperty "message" properties
           times    <- return (extractProperty "times" properties)
           lastUsed <- return (extractProperty "lastUsed" properties)
           return CustomCommand { customCommandName = name
                                , customCommandMessage = message
                                , customCommandTimes = fromMaybe 0 times
                                , customCommandLastUsed = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) lastUsed
                                }

customCommandByName :: T.Text -> MaybeT Effect (Entity CustomCommand)
customCommandByName name =
    MaybeT
      $ fmap (listToMaybe >=> fromEntityProperties)
      $ selectEntities "CustomCommand"
      $ Filter (PropertyEquals "name" $ PropertyText name) All

addCustomCommand :: CommandTable a -> CommandHandler (T.Text, T.Text)
addCustomCommand builtinCommands sender (name, message) =
    do customCommand  <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       case (customCommand, builtinCommand) of
         (Just _, Nothing) ->
           replyToUser (senderName sender)
             $ T.pack
             $ printf "Command '%s' already exists" name
         (Nothing, Just _) ->
           replyToUser (senderName sender)
             $ T.pack
             $ printf "There is already a builtin command with name '%s'" name
         (Just _, Just _) ->
             errorEff
               $ T.pack
               $ printf "Custom command '%s' collide with a built in command"
         (Nothing, Nothing) ->
             do _ <- createEntity "CustomCommand" CustomCommand { customCommandName = name
                                                                , customCommandMessage = message
                                                                , customCommandTimes = 0
                                                                , customCommandLastUsed = UTCTime (ModifiedJulianDay 0) 0
                                                                }
                replyToUser (senderName sender) $ T.pack $ printf "Add command '%s'" name

deleteCustomCommand :: CommandTable a -> CommandHandler T.Text
deleteCustomCommand builtinCommands sender name =
    do customCommand  <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       case (customCommand, builtinCommand) of
         (Just _, Nothing) ->
             do _ <- deleteEntities "CustomCommand" (Filter (PropertyEquals "name" $ PropertyText name) All)
                replyToSender sender
                  $ T.pack
                  $ printf "Command '%s' has been removed" name
         (Nothing, Just _) ->
             replyToSender sender
               $ T.pack
               $ printf "Command '%s' is builtin and can't be removed like that" name
         (Just _, Just _) ->
             errorEff
               $ T.pack
               $ printf "Custom command '%s' collide with a built in command"
         (Nothing, Nothing) ->
             replyToSender sender
               $ T.pack
               $ printf "Command '%s' does not exist" name

updateCustomCommand :: CommandTable a -> CommandHandler (T.Text, T.Text)
updateCustomCommand builtinCommands sender (name, message) =
    do customCommand <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       case (customCommand, builtinCommand) of
         (Just cmd, Nothing) ->
             do _ <- updateEntityById (replaceCustomCommandMessage message <$> cmd)
                replyToSender sender $ T.pack $ printf "Command '%s' has been updated" name
         (Nothing, Just _) ->
             replyToSender sender
               $ T.pack
               $ printf "Command '%s' is builtin and can't be updated like that" name
         (Just _, Just _) ->
             errorEff
               $ T.pack
               $ printf "Custom command '%s' collide with a built in command"
         (Nothing, Nothing) ->
             replyToSender sender
               $ T.pack
               $ printf "Command '%s' does not exist" name

expandCustomCommandVars :: CustomCommand -> Effect CustomCommand
expandCustomCommandVars customCommand = do
  let message = customCommandMessage customCommand
  let times = customCommandTimes customCommand
  expandedMessage <- expandVariables $ T.replace "%times" (T.pack $ show times) message
  return $ customCommand { customCommandMessage = expandedMessage }

bumpCustomCommandTimes :: CustomCommand -> CustomCommand
bumpCustomCommandTimes customCommand =
    customCommand { customCommandTimes = customCommandTimes customCommand + 1 }

replaceCustomCommandMessage :: T.Text -> CustomCommand -> CustomCommand
replaceCustomCommandMessage message customCommand =
    customCommand { customCommandMessage = message }

customCommandCooldown :: Double -> Entity CustomCommand -> MaybeT Effect (Entity CustomCommand)
customCommandCooldown cooldownTimeout customCommand =
    do currentTime <- lift now
       diffTime    <- return
                        $ diffUTCTime currentTime
                        $ customCommandLastUsed
                        $ entityPayload customCommand

       if realToFrac diffTime > cooldownTimeout
       then MaybeT
              $ return
              $ Just
              $ fmap (\cmd -> cmd { customCommandLastUsed = currentTime }) customCommand
       else do lift
                 $ logMsg
                 $ T.pack
                 $ printf "Command '%s' has not cooled down yet"
                 $ customCommandName
                 $ entityPayload customCommand
               MaybeT $ return Nothing


{-# ANN dispatchCustomCommand ("HLint: ignore Use fmap" :: String) #-}
{-# ANN dispatchCustomCommand ("HLint: ignore Use <$>" :: String) #-}
dispatchCustomCommand :: Sender -> Command T.Text -> Effect ()
dispatchCustomCommand _ command =
  do customCommand <- runMaybeT (customCommandByName (commandName command)
                                   >>= customCommandCooldown 2.0
                                   >>= return . fmap bumpCustomCommandTimes
                                   >>= MaybeT . updateEntityById
                                   >>= return . entityPayload
                                   >>= lift   . expandCustomCommandVars)
     maybe (return ())
           (say . customCommandMessage)
           customCommand
