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
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport

data CustomCommand = CustomCommand
  { customCommandName :: T.Text
  , customCommandMessage :: T.Text
  , customCommandTimes :: Int
  }

instance IsEntity CustomCommand where
  nameOfEntity _ = "CustomCommand"
  toProperties customCommand =
    M.fromList
      [ ("name", PropertyText $ customCommandName customCommand)
      , ("message", PropertyText $ customCommandMessage customCommand)
      , ("times", PropertyInt $ customCommandTimes customCommand)
      ]
  fromProperties properties =
    CustomCommand <$> extractProperty "name" properties <*>
    extractProperty "message" properties <*>
    pure (fromMaybe 0 $ extractProperty "times" properties)

customCommandByName :: T.Text -> MaybeT Effect (Entity CustomCommand)
customCommandByName name =
  MaybeT $
  fmap listToMaybe $
  selectEntities "CustomCommand" $
  Filter (PropertyEquals "name" $ PropertyText name) All

addCustomCommand :: CommandTable -> Reaction Message (T.Text, T.Text)
addCustomCommand builtinCommands =
  Reaction $ \Message {messageSender = sender, messageContent = (name, message)} -> do
    customCommand <- runMaybeT $ customCommandByName name
    let builtinCommand = M.lookup name builtinCommands
    case (customCommand, builtinCommand) of
      (Just _, Nothing) ->
        replyToSender sender [qms|Command '{name}' already exists|]
      (Nothing, Just _) ->
        replyToSender
          sender
          [qms|There is already a builtin command with name '{name}'|]
      (Just _, Just _) ->
        errorEff [qms|Custom command '{name}' collide with a built in command|]
      (Nothing, Nothing) -> do
        void $
          createEntity
            "CustomCommand"
            CustomCommand
              { customCommandName = name
              , customCommandMessage = message
              , customCommandTimes = 0
              }
        replyToSender sender [qms|Added command '{name}'|]

deleteCustomCommand :: CommandTable -> Reaction Message T.Text
deleteCustomCommand builtinCommands =
  Reaction $ \Message {messageSender = sender, messageContent = name} -> do
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
          [qms|Command '{name}' is builtin and can't be removed like that|]
      (Just _, Just _) ->
        errorEff
          [qms|Custom command '{name}'
             collide with a built in command|]
      (Nothing, Nothing) ->
        replyToSender sender [qms|Command '{name}' does not exist|]

showCustomCommand :: CommandTable -> Reaction Message T.Text
showCustomCommand builtinCommands =
  Reaction $ \Message {messageContent = name, messageSender = sender} -> do
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

timesCustomCommand :: CommandTable -> Reaction Message T.Text
timesCustomCommand builtinCommands =
  Reaction $ \Message {messageSender = sender, messageContent = name} -> do
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

updateCustomCommand :: CommandTable -> Reaction Message (T.Text, T.Text)
updateCustomCommand builtinCommands =
  Reaction $ \Message {messageSender = sender, messageContent = (name, message)} -> do
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
  timestamp <- now
  let day = utctDay timestamp
  let (yearNum, monthNum, dayNum) = toGregorian day
  let message = customCommandMessage customCommand
  let times = customCommandTimes customCommand
  let vars =
        [ ("%times", [qms|{times}|])
        , ("%year", [qms|{yearNum}|])
        , ("%month", [qms|{monthNum}|])
        , ("%day", [qms|{dayNum}|])
        , ("%date", [qms|{showGregorian day}|])
        , ("%sender", mentionSender sender)
        , ("%1", args)
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
      (customCommandByName cmd >>= return . fmap bumpCustomCommandTimes >>=
       MaybeT . updateEntityById >>=
       return . entityPayload >>=
       lift . expandCustomCommandVars sender args)
  maybe
    (return ())
    (say (senderChannel sender) . customCommandMessage)
    customCommand
