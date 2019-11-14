{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.CustomCommand
  ( addCustomCommand
  , deleteCustomCommand
  , dispatchCustomCommand
  , updateCustomCommand
  , showCustomCommand
  , timesCustomCommand
  , CustomCommand(..)
  ) where

import Bot.Expr
import Bot.Replies
import Command
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Functor.Compose
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import HyperNerd.Parser
import qualified Network.URI.Encode as URI
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
  selectEntities Proxy $ Filter (PropertyEquals "name" $ PropertyText name) All

-- TODO(#815): CRUD custom command should update help page now they're listed there as well.
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
            Proxy
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
          deleteEntities (Proxy :: Proxy CustomCommand) $
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
      (Nothing, Just bc) ->
        replyToSender
          sender
          [qms|Command '{name}' is builtin. Look into the code
               for the definition: {bcGitHubLocation bc}|]
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

evalExpr :: M.Map T.Text T.Text -> Expr -> T.Text
evalExpr _ (TextExpr t) = t
evalExpr vars (FunCallExpr "or" args) =
  fromMaybe "" $ listToMaybe $ dropWhile T.null $ map (evalExpr vars) args
evalExpr vars (FunCallExpr "urlencode" args) =
  T.concat $ map (T.pack . URI.encode . T.unpack . evalExpr vars) args
evalExpr vars (FunCallExpr funame _) = fromMaybe "" $ M.lookup funame vars

expandVars :: M.Map T.Text T.Text -> [Expr] -> T.Text
expandVars vars = T.concat . map (evalExpr vars)

-- TODO(#598): reimplement expandCustomCommandVars with Bot.Expr when it's ready
expandCustomCommandVars ::
     Message (Command T.Text, Entity CustomCommand)
  -> Effect (Either String CustomCommand)
expandCustomCommandVars Message { messageSender = sender
                                , messageContent = (Command {commandArgs = args}, Entity {entityPayload = customCommand})
                                } = do
  timestamp <- now
  let day = utctDay timestamp
  let (yearNum, monthNum, dayNum) = toGregorian day
  let code = runParser exprs $ customCommandMessage customCommand
  let times = customCommandTimes customCommand
  let vars =
        M.fromList
          [ ("times", [qms|{times}|])
          , ("year", [qms|{yearNum}|])
          , ("month", [qms|{monthNum}|])
          , ("day", [qms|{dayNum}|])
          , ("date", [qms|{showGregorian day}|])
          , ("sender", mentionSender sender)
          , ("1", args)
          ]
  case code of
    Left msg -> return $ Left (show msg)
    Right (_, code') ->
      return $
      Right customCommand {customCommandMessage = expandVars vars code'}

bumpCustomCommandTimes :: CustomCommand -> CustomCommand
bumpCustomCommandTimes customCommand =
  customCommand {customCommandTimes = customCommandTimes customCommand + 1}

replaceCustomCommandMessage :: T.Text -> CustomCommand -> CustomCommand
replaceCustomCommandMessage message customCommand =
  customCommand {customCommandMessage = message}

dispatchCustomCommand :: Reaction Message (Command T.Text)
dispatchCustomCommand =
  liftFst (runMaybeT . customCommandByName . commandName) $
  cmapR f $
  ignoreNothing $
  transR Compose $
  liftR (updateEntityById . fmap bumpCustomCommandTimes) $
  ignoreNothing $
  transR getCompose $
  dupLiftR expandCustomCommandVars $
  replyLeft $ cmapR customCommandMessage sayMessage
  where
    f :: Functor m => (a, m b) -> m (a, b)
    f = uncurry $ fmap . (,)
