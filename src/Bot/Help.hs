{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Help
  ( helpCommand
  , setHelpGistId
  , refreshHelpGistId
  , startRefreshHelpGistTimer
  ) where

import Bot.CustomCommand
import Bot.GitHub
import Bot.Replies
import Command
import Data.Bool.Extra
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Effect
import Entity
import Property
import Reaction
import Text.InterpolatedString.QM
import Transport

data HelpState = HelpState
  { helpStateGistId :: Maybe GistId
  , helpStateGistFresh :: Bool
  }

updateHelpStateGistId :: GistId -> HelpState -> HelpState
updateHelpStateGistId (GistId "") state = state {helpStateGistId = Nothing}
updateHelpStateGistId gistId state = state {helpStateGistId = Just gistId}

updateHelpStateGistFresh :: Bool -> HelpState -> HelpState
updateHelpStateGistFresh gistFresh state =
  state {helpStateGistFresh = gistFresh}

instance IsEntity HelpState where
  nameOfEntity _ = "HelpState"
  toProperties state =
    M.fromList
      (("gistFresh", PropertyInt $ boolAsInt $ helpStateGistFresh state) :
       maybeToList
         ((,) "gistId" . PropertyText . gistIdAsText <$> helpStateGistId state))
  fromProperties properties =
    HelpState <$> pure (GistId <$> extractProperty "gistId" properties) <*>
    pure (maybe False intAsBool $ extractProperty "gistFresh" properties)

currentHelpState :: Effect (Entity HelpState)
currentHelpState = do
  state <- listToMaybe <$> selectEntities Proxy All
  case state of
    Just state' -> return state'
    Nothing -> createEntity Proxy $ HelpState Nothing False

setHelpGistId :: Reaction Message T.Text
setHelpGistId =
  liftR
    (\gistId -> do
       state <- currentHelpState
       updateEntityById (updateHelpStateGistId (GistId gistId) <$> state)) $
  cmapR (const "Update Gist ID for Help Page") $ Reaction replyMessage

refreshHelpGistId :: Reaction Message a
refreshHelpGistId =
  liftR (const currentHelpState) $
  cmapR (updateHelpStateGistFresh False <$>) $
  liftR updateEntityById $
  cmapR (const "Scheduled to refresh the Help Gist Page") $
  Reaction replyMessage

gistRenderCommandTable :: CommandTable -> T.Text
gistRenderCommandTable =
  ("* Builtin Commands \n" <>) . T.unlines . map renderRow . M.toList
  where
    renderRow (name, command) =
      [qms||{name}|{bcDescription command}|{bcGitHubLocation command}||]

gistRenderCustomCommandsTable :: [Entity CustomCommand] -> T.Text
gistRenderCustomCommandsTable =
  ("* Custom commands \n" <>) . T.unlines . map (renderRow . entityPayload)
  where
    renderRow (CustomCommand name message times) =
      [qms||{name}|{message}|{times}||]

refreshHelpGist :: CommandTable -> GistId -> Effect ()
refreshHelpGist commandTable gistId = do
  customsList <- selectEntities Proxy All
  updateGistFile
    helpGistFileName
    (FileContent
       (gistRenderCommandTable commandTable <> "\n" <>
        gistRenderCustomCommandsTable customsList))
    gistId

startRefreshHelpGistTimer :: CommandTable -> Effect ()
startRefreshHelpGistTimer commandTable =
  periodicEffect period Nothing $ do
    state <- currentHelpState
    case helpStateGistId $ entityPayload state of
      Just gistId
        | not $ helpStateGistFresh $ entityPayload state -> do
          logMsg "[INFO] Help Gist is not Fresh. Updating.."
          refreshHelpGist commandTable gistId
          void $ updateEntityById (updateHelpStateGistFresh True <$> state)
      Nothing -> logMsg "[INFO] Gist ID is not setup for Help Page"
      _ -> logMsg "[INFO] Help Gist is fresh AF 👌"
  where
    period = 60 * 1000

helpCommand :: CommandTable -> Reaction Message T.Text
helpCommand commandTable =
  ifR
    T.null
    (replyAvaliableCommands commandTable)
    (replyHelpForCommand commandTable)

replyHelpForCommand :: CommandTable -> Reaction Message T.Text
replyHelpForCommand commandTable =
  cmapR T.strip $
  cmapR (`M.lookup` commandTable) $
  replyOnNothing "Cannot find such command FeelsBadMan" $
  cmapR (\bc -> [qms|{bcDescription bc} | Located in {bcGitHubLocation bc}|]) $
  Reaction replyMessage

helpGistFileName :: FileName
helpGistFileName = FileName "Help.org"

helpGistUrl :: GistId -> T.Text
helpGistUrl (GistId gistId) =
  [qms|https://gist.github.com/{gistId}#file-{gistFileAnchor helpGistFileName}|]

replyAvaliableCommands :: CommandTable -> Reaction Message T.Text
replyAvaliableCommands _ =
  liftR (const currentHelpState) $
  cmapR (helpStateGistId . entityPayload) $
  replyOnNothing "Admin did not setup Gist Page for Help" $
  cmapR helpGistUrl $ Reaction replyMessage
