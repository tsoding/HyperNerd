{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import           Text.InterpolatedString.QM

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
        CustomCommand <$> extractProperty "name" properties
                      <*> extractProperty "message" properties
                      <*> pure (fromMaybe 0 $ extractProperty "times" properties)
                      <*> pure (fromMaybe dayZero $ extractProperty "lastUsed" properties)
        where dayZero = UTCTime (ModifiedJulianDay 0) 0

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
         (Just _, Nothing) -> replyToSender sender [qms|Command '{name}' already
                                                        exists|]
         (Nothing, Just _) -> replyToSender sender [qms|There is already a builtin
                                                        command with name '{name}'|]
         (Just _, Just _)  -> errorEff [qms|Custom command '{name}' collide with
                                            a built in command|]
         (Nothing, Nothing) ->
             do void $ createEntity "CustomCommand" CustomCommand { customCommandName = name
                                                                  , customCommandMessage = message
                                                                  , customCommandTimes = 0
                                                                  , customCommandLastUsed = UTCTime (ModifiedJulianDay 0) 0
                                                                  }
                replyToSender sender [qms|Added command '{name}'|]

deleteCustomCommand :: CommandTable a -> CommandHandler T.Text
deleteCustomCommand builtinCommands sender name =
    do customCommand  <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       case (customCommand, builtinCommand) of
         (Just _, Nothing) ->
             do void $
                  deleteEntities "CustomCommand" $
                  Filter (PropertyEquals "name" $
                          PropertyText name) All
                replyToSender sender [qms|Command '{name}' has been removed|]
         (Nothing, Just _)  -> replyToSender sender [qms|Command '{name}' is builtin
                                                         and can't be removed like that|]
         (Just _, Just _)   -> errorEff [qms|Custom command '{name}' collide with a
                                             built in command|]
         (Nothing, Nothing) -> replyToSender sender [qms|Command '{name}' does not exist|]

updateCustomCommand :: CommandTable a -> CommandHandler (T.Text, T.Text)
updateCustomCommand builtinCommands sender (name, message) =
    do customCommand <- runMaybeT $ customCommandByName name
       builtinCommand <- return $ M.lookup name builtinCommands

       case (customCommand, builtinCommand) of
         (Just cmd, Nothing) ->
             do void $ updateEntityById (replaceCustomCommandMessage message <$> cmd)
                replyToSender sender [qms|Command '{name}' has been updated|]
         (Nothing, Just _)  -> replyToSender sender [qms|Command '{name}' is builtin and
                                                         can't be updated like that|]
         (Just _, Just _)   -> errorEff [qms|Custom command '{name}' collide with
                                            a built in command|]
         (Nothing, Nothing) -> replyToSender sender [qms|Command '{name}' does not exist|]

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
       else do let name = customCommandName $ entityPayload customCommand
               lift $ logMsg [qms|Command '{name}' has not cooled down yet|]
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
