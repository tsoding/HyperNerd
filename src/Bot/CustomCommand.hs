{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.CustomCommand
  ( addCustomCommand
  , deleteCustomCommand
  , dispatchCustomCommand
  , updateCustomCommand
  , showCustomCommand
  , timesCustomCommand
  ) where

import Bot.Replies
import Bot.Variable
import Command
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Events
import Property
import Text.InterpolatedString.QM
import Data.Bool (boolAsInt, intAsBool)

data CustomCommand = CustomCommand
  { customCommandName :: T.Text
  , customCommandMessage :: T.Text
  , customCommandTimes :: Int
  , customCommandLastUsed :: UTCTime
  , customCommandEscape :: Bool
  }

instance IsEntity CustomCommand where
  toProperties customCommand =
    M.fromList
      [ ("name", PropertyText $ customCommandName customCommand)
      , ("message", PropertyText $ customCommandMessage customCommand)
      , ("times", PropertyInt $ customCommandTimes customCommand)
      , ("lastUsed", PropertyUTCTime $ customCommandLastUsed customCommand)
      , ("escape", PropertyInt $ boolAsInt $ customCommandEscape customCommand)
      ]
  fromProperties properties =
    CustomCommand <$> extractProperty "name" properties <*>
    extractProperty "message" properties <*>
    pure (fromMaybe 0 $ extractProperty "times" properties) <*>
    pure (fromMaybe dayZero $ extractProperty "lastUsed" properties) <*>
    pure (maybe True intAsBool $ extractProperty "escape" properties)
    where
      dayZero = UTCTime (ModifiedJulianDay 0) 0

customCommandByName :: T.Text -> MaybeT Effect (Entity CustomCommand)
customCommandByName name =
  MaybeT $
  fmap (listToMaybe >=> fromEntityProperties) $
  selectEntities "CustomCommand" $
  Filter (PropertyEquals "name" $ PropertyText name) All

addCustomCommand :: CommandTable -> CommandHandler (T.Text, T.Text)
addCustomCommand builtinCommands Message { messageSender = sender
                                         , messageContent = (name, message)
                                         } = do
  customCommand <- runMaybeT $ customCommandByName name
  let builtinCommand = M.lookup name builtinCommands
  case (customCommand, builtinCommand) of
    (Just _, Nothing) ->
      replyToSender
        sender
        [qms|Command '{name}' already
                                                   exists|]
    (Nothing, Just _) ->
      replyToSender
        sender
        [qms|There is already a builtin
                                                   command with name '{name}'|]
    (Just _, Just _) ->
      errorEff
        [qms|Custom command '{name}' collide with
                                       a built in command|]
    (Nothing, Nothing) -> do
      void $
        createEntity
          "CustomCommand"
          CustomCommand
            { customCommandName = name
            , customCommandMessage = message
            , customCommandTimes = 0
            , customCommandLastUsed = UTCTime (ModifiedJulianDay 0) 0
            , customCommandEscape = True
            }
      replyToSender sender [qms|Added command '{name}'|]

deleteCustomCommand :: CommandTable -> CommandHandler T.Text
deleteCustomCommand builtinCommands Message { messageSender = sender
                                            , messageContent = name
                                            } = do
  customCommand <- runMaybeT $ customCommandByName name
  let builtinCommand = M.lookup name builtinCommands
  case (customCommand, builtinCommand) of
    (Just _, Nothing) -> do
      void $
        deleteEntities "CustomCommand" $
        Filter (PropertyEquals "name" $ PropertyText name) All
      replyToSender sender [qms|Command '{name}' has been removed|]
    (Nothing, Just _) ->
      replyToSender
        sender
        [qms|Command '{name}' is builtin
                                                    and can't be removed like that|]
    (Just _, Just _) ->
      errorEff
        [qms|Custom command '{name}' collide with a
                                        built in command|]
    (Nothing, Nothing) ->
      replyToSender sender [qms|Command '{name}' does not exist|]

showCustomCommand :: CommandTable -> CommandHandler T.Text
showCustomCommand builtinCommands Message { messageContent = name
                                          , messageSender = sender
                                          } = do
  customCommand <- runMaybeT $ customCommandByName name
  let builtinCommand = M.lookup name builtinCommands
  case (customCommand, builtinCommand) of
    (Just cmd, Nothing) ->
      replyToSender
        sender
        [qms|Command '{name}' defined as
                                                     '{customCommandMessage $ entityPayload cmd}'|]
    (Nothing, Just _) ->
      replyToSender
        sender
        [qms|Command '{name}' is builtin. Look into the code
                                                     for the definition: https://github.com/tsoding/HyperNerd|]
    (Just _, Just _) ->
      errorEff
        [qms|Custom command '{name}' collide with
                                         a built in command|]
    (Nothing, Nothing) ->
      replyToSender sender [qms|Command '{name}' does not exist|]

timesCustomCommand :: CommandTable -> CommandHandler T.Text
timesCustomCommand builtinCommands Message { messageSender = sender
                                           , messageContent = name
                                           } = do
  customCommand <- runMaybeT $ customCommandByName name
  let builtinCommand = M.lookup name builtinCommands
  case (customCommand, builtinCommand) of
    (Just cmd, Nothing) ->
      replyToSender
        sender
        [qms|Command '{name}' was invoked
                                                     {customCommandTimes $ entityPayload cmd} times.|]
    (Nothing, Just _) ->
      replyToSender
        sender
        [qms|Command '{name}' is builtin and
                                                    we don't track the frequency usage for builtin commands.
                                                    See https://github.com/tsoding/HyperNerd/issues/334
                                                    for more info.|]
    (Just _, Just _) ->
      errorEff
        [qms|Custom command '{name}' collide with
                                        a built in command|]
    (Nothing, Nothing) ->
      replyToSender sender [qms|Command '{name}' does not exist|]

updateCustomCommand :: CommandTable -> CommandHandler (T.Text, T.Text)
updateCustomCommand builtinCommands Message { messageSender = sender
                                            , messageContent = (name, message)
                                            } = do
  customCommand <- runMaybeT $ customCommandByName name
  let builtinCommand = M.lookup name builtinCommands
  case (customCommand, builtinCommand) of
    (Just cmd, Nothing) -> do
      void $ updateEntityById (replaceCustomCommandMessage message <$> cmd)
      replyToSender sender [qms|Command '{name}' has been updated|]
    (Nothing, Just _) ->
      replyToSender
        sender
        [qms|Command '{name}' is builtin and
                                                    can't be updated like that|]
    (Just _, Just _) ->
      errorEff
        [qms|Custom command '{name}' collide with
                                        a built in command|]
    (Nothing, Nothing) ->
      replyToSender sender [qms|Command '{name}' does not exist|]

expandCustomCommandVars ::
     Sender -> T.Text -> CustomCommand -> Effect CustomCommand
expandCustomCommandVars sender args customCommand = do
  (year, month, day) <- toGregorian . utctDay <$> now
  let message = customCommandMessage customCommand
  let times = customCommandTimes customCommand
  let vars =
        [ ("%times", [qms|{times}|])
        , ("%1", args)
        , ("%year", [qms|{year}|])
        , ("%month", [qms|{month}|])
        , ("%day", [qms|{day}|])
        , ("%date", [qms|{day}.{month}.{year}|])
        , ("%sender", [qms|{senderName sender}|])
        ]
  expandedMessage <-
    expandVariables $ foldl (flip $ uncurry T.replace) message vars
  return $ customCommand {customCommandMessage = expandedMessage}

bumpCustomCommandTimes :: CustomCommand -> CustomCommand
bumpCustomCommandTimes customCommand =
  customCommand {customCommandTimes = customCommandTimes customCommand + 1}

replaceCustomCommandMessage :: T.Text -> CustomCommand -> CustomCommand
replaceCustomCommandMessage message customCommand =
  customCommand {customCommandMessage = message}

customCommandCooldown ::
     Double -> Entity CustomCommand -> MaybeT Effect (Entity CustomCommand)
customCommandCooldown cooldownTimeout customCommand = do
  currentTime <- lift now
  let diffTime =
        diffUTCTime currentTime $
        customCommandLastUsed $ entityPayload customCommand
  if realToFrac diffTime > cooldownTimeout
    then MaybeT $
         return $
         Just $
         fmap (\cmd -> cmd {customCommandLastUsed = currentTime}) customCommand
    else do
      let name = customCommandName $ entityPayload customCommand
      lift $ logMsg [qms|Command '{name}' has not cooled down yet|]
      MaybeT $ return Nothing

executeCustomCommand :: CustomCommand -> Effect ()
executeCustomCommand customCommand =
  if customCommandEscape customCommand
    then escapeSay commandEffect
    else commandEffect
  where
    commandEffect = say $ customCommandMessage customCommand

{-# ANN dispatchCustomCommand ("HLint: ignore Use fmap" :: String)
        #-}

{-# ANN dispatchCustomCommand ("HLint: ignore Use <$>" :: String)
        #-}

dispatchCustomCommand :: Message (Command T.Text) -> Effect ()
dispatchCustomCommand Message { messageContent = Command { commandName = cmd
                                                         , commandArgs = args
                                                         }
                              , messageSender = sender
                              } = do
  customCommand <-
    runMaybeT
      (customCommandByName cmd >>= customCommandCooldown 2.0 >>=
       return . fmap bumpCustomCommandTimes >>=
       MaybeT . updateEntityById >>=
       return . entityPayload >>=
       lift . expandCustomCommandVars sender args)
  maybe (return ()) executeCustomCommand customCommand
