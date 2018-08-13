{-# LANGUAGE OverloadedStrings #-}
module Bot.CustomCommand ( addCustomCommand
                         , deleteCustomCommand
                         , dispatchCustomCommand
                         , updateCustomCommand
                         ) where

import           Bot.Replies
import           Command
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Property
import           Text.Printf

data CustomCommand = CustomCommand { customCommandName :: T.Text
                                   , customCommandMessage :: T.Text
                                   , customCommandTimes :: Int
                                   }

instance IsEntity CustomCommand where
    toProperties customCommand =
        M.fromList [ ("name", PropertyText $ customCommandName customCommand)
                   , ("message", PropertyText $ customCommandMessage customCommand)
                   , ("times", PropertyInt $ customCommandTimes customCommand)
                   ]
    fromProperties entity = do name    <- extractProperty "name" entity
                               message <- extractProperty "message" entity
                               times   <- return (extractProperty "times" entity)
                               customCommand <- return CustomCommand { customCommandName = name
                                                                     , customCommandMessage = message
                                                                     , customCommandTimes = fromMaybe 0 times
                                                                     }
                               return (const customCommand <$> entity)

{-# ANN customCommandByName ("HLint: ignore Use <$>" :: String) #-}
customCommandByName :: T.Text -> MaybeT Effect (Entity CustomCommand)
customCommandByName name =
    MaybeT
      $ fmap (listToMaybe >=> fromProperties)
      $ selectEntities "CustomCommand"
      $ Filter (PropertyEquals "name" $ PropertyText name) All

addCustomCommand :: CommandTable a -> CommandHandler (T.Text, T.Text)
addCustomCommand builtinCommands sender (name, message) =
    do customCommand  <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       if isJust customCommand || isJust builtinCommand
       then replyToUser (senderName sender)
              $ T.pack
              $ printf "Command '%s' already exists" name
       else do _ <- createEntity "CustomCommand" CustomCommand { customCommandName = name
                                                               , customCommandMessage = message
                                                               , customCommandTimes = 0
                                                               }
               replyToUser (senderName sender) $ T.pack $ printf "Add command '%s'" name

deleteCustomCommand :: CommandTable a -> CommandHandler T.Text
deleteCustomCommand builtinCommands sender name =
    do customCommand  <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       if isJust customCommand
       then do _ <- deleteEntities "CustomCommand" (Filter (PropertyEquals "name" $ PropertyText name) All)
               replyToSender sender $ T.pack $ printf "Command '%s' has been removed" name
       else if isJust builtinCommand
            then replyToSender sender $ T.pack $ printf "Command '%s' is builtin and can't be removed like that" name
            else replyToSender sender $ T.pack $ printf "Command '%s' does not exist" name

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

expandCustomCommandVars :: CustomCommand -> CustomCommand
expandCustomCommandVars customCommand =
    customCommand { customCommandMessage = T.replace "%times" (T.pack $ show times) message }
    where message = customCommandMessage customCommand
          times = customCommandTimes customCommand

bumpCustomCommandTimes :: CustomCommand -> CustomCommand
bumpCustomCommandTimes customCommand =
    customCommand { customCommandTimes = customCommandTimes customCommand + 1 }

replaceCustomCommandMessage :: T.Text -> CustomCommand -> CustomCommand
replaceCustomCommandMessage message customCommand =
    customCommand { customCommandMessage = message }

{-# ANN dispatchCustomCommand ("HLint: ignore Use fmap" :: String) #-}
{-# ANN dispatchCustomCommand ("HLint: ignore Use <$>" :: String) #-}
dispatchCustomCommand :: Sender -> Command T.Text -> Effect ()
dispatchCustomCommand _ command =
  do customCommand <- runMaybeT (customCommandByName (commandName command)
                                   >>= return . fmap bumpCustomCommandTimes
                                   >>= MaybeT . updateEntityById
                                   >>= return . fmap expandCustomCommandVars)
     maybe (return ())
           (say . customCommandMessage . entityPayload)
           customCommand

-- TODO(#170): There is no way to update a custom command
