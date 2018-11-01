{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot
  ( Bot
  , bot
  , Event(..)
  , Sender(..)
  , TwitchStream(..)
  ) where

import Bot.Alias
import Bot.BttvFfz
import Bot.CustomCommand
import Bot.Dubtrack
import Bot.Links
import Bot.Log
import Bot.Periodic
import Bot.Poll
import Bot.Quote
import Bot.Replies
import Bot.Russify
import Bot.Twitch
import Bot.Variable
import Command
import Control.Monad
import Data.Array
import Data.Char
import Data.Either
import Data.Foldable
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Effect
import Entity
import Events
import Reaction
import Safe
import Text.InterpolatedString.QM
import Text.Read
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

type Bot = Event -> Effect ()

builtinCommands :: CommandTable
builtinCommands =
  M.fromList
    [ ("russify", ("Russify western spy text", russifyCommand))
    , ( "addquote"
      , ( "Add quote to quote database"
        , authorizeSender
            (\sender -> senderMod sender || senderSubscriber sender) $
          replyOnNothing
            "Only subs and mods can add quotes, sorry."
            addQuoteCommand))
    , ( "delquote"
      , ( "Delete quote from quote database"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          cmapR (readMaybe . T.unpack) $
          replyOnNothing "Expected integer as an argument" deleteQuoteCommand))
    , ( "quote"
      , ( "Get a quote from the quote database"
        , Reaction $ readCommand quoteCommand))
    , ("bttv", ("Show all available BTTV emotes", cmapR (const ()) bttvCommand))
    , ("ffz", ("Show all available FFZ emotes", cmapR (const ()) ffzCommand))
    , ( "updateffz"
      , ( "Update FFZ cache"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          cmapR (const ()) updateFfzEmotesCommand))
    , ( "updatebttv"
      , ( "Update BTTV cache"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          cmapR (const ()) updateBttvEmotesCommand))
    , ("help", ("Send help", Reaction $ helpCommand builtinCommands))
    , ( "poll"
      , ( "Starts a poll"
        , Reaction $
          modCommand $
          regexArgsCommand "([0-9]+) (.*)" $
          pairArgsCommand $
          contramapCH
            (\(duration, options) ->
               fmap (, T.words options) $ readMaybe $ T.unpack duration) $
          justCommand pollCommand))
    , ( "cancelpoll"
      , ( "Cancels the current poll"
        , Reaction $ modCommand $ voidCommand cancelPollCommand))
    , ( "checkpoll"
      , ("", Reaction $ modCommand $ voidCommand currentPollCommand))
    , ( "vote"
      , ( "Vote for a poll option"
        , cmapR T.words $ cmapR headMay $ ignoreNothing voteCommand))
    , ("uptime", ("Show stream uptime", Reaction $ voidCommand uptimeCommand))
    , ( "rq"
      , ("Get random quote from your log", Reaction randomLogRecordCommand))
    , ( "addperiodic"
      , ( "Add periodic command"
        , Reaction $ modCommand $ commandArgsCommand addPeriodicCommand))
    , ( "delperiodic"
      , ("Delete periodic command", Reaction $ modCommand removePeriodicCommand))
    , ( "addcmd"
      , ( "Add custom command"
        , Reaction $
          modCommand $
          regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
          pairArgsCommand $ addCustomCommand builtinCommands))
    , ( "delcmd"
      , ( "Delete custom command"
        , Reaction $ modCommand $ deleteCustomCommand builtinCommands))
    , ( "updcmd"
      , ( "Update custom command"
        , Reaction $
          modCommand $
          regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
          pairArgsCommand $ updateCustomCommand builtinCommands))
               -- TODO(#337): use help instead of !showcmd
    , ( "showcmd"
      , ( "Show custom command definition"
        , Reaction $
          regexArgsCommand "([a-zA-Z0-9]+)" $
          firstArgCommand $ showCustomCommand builtinCommands))
    , ( "timescmd"
      , ( "Show amount of times the custom commands was invoked"
        , Reaction $
          regexArgsCommand "([a-zA-Z0-9]+)" $
          firstArgCommand $ timesCustomCommand builtinCommands))
    , ( "song"
      , ( "Print currently playing song"
        , Reaction $ voidCommand currentSongCommand))
    , ( "addalias"
      , ( "Add command alias"
        , Reaction $
          modCommand $
          regexArgsCommand "([a-zA-Z0-9]+) ([a-zA-Z0-9]+)" $
          pairArgsCommand addAliasCommand))
    , ( "delalias"
      , ("Remove command alias", Reaction $ modCommand removeAliasCommand))
    , ( "addvar"
      , ( "Add variable"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" addVariable))
    , ( "updvar"
      , ( "Update variable"
        , Reaction $
          modCommand $
          regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
          pairArgsCommand $ runReaction updateVariable))
    , ( "delvar"
      , ("Delete variable", Reaction $ modCommand $ runReaction deleteVariable))
    , ( "nuke"
      , ( [qms|Looks at N previous messages and bans all of
                                the users whose messages match provided regex|]
        , Reaction $
          modCommand $
          regexArgsCommand "([0-9]+) (.*)" $
          pairArgsCommand $ \Message {messageContent = (strN, regexStr)} -> do
            let parsedN =
                  maybe (Left "Could not parse N") Right $
                  readMaybe $ T.unpack strN
            let compiledRegex =
                  compile defaultCompOpt defaultExecOpt $ T.unpack regexStr
            case liftM2 (,) parsedN compiledRegex of
              Left msg ->
                logMsg
                  [qms|[WARNING] Could not parse
                                                               arguments: {msg}|]
              Right (n, regex) -> do
                logs <-
                  selectEntities "LogRecord" $
                  Take n $ SortBy "timestamp" Desc All
                traverse_ (banUser . lrUser . entityPayload) $
                  filter
                    (isRight . execute regex . T.unpack . lrMsg . entityPayload)
                    logs))
    , ( "cycle"
      , ("Mock the message", Reaction (replyMessage . fmap mockMessage)))
    , ( "trust"
      , ( "Makes the user trusted"
        , Reaction $
          modCommand $ regexArgsCommand "(.+)" $ firstArgCommand trustCommand))
    , ( "untrust"
      , ( "Untrusts the user"
        , Reaction $
          modCommand $ regexArgsCommand "(.+)" $ firstArgCommand untrustCommand))
    , ( "amitrusted"
      , ( "Check if you are a trusted user"
        , Reaction $ voidCommand amitrustedCommand))
    , ( "istrusted"
      , ( "Check if the user is trusted"
        , Reaction $ regexArgsCommand "(.+)" $ firstArgCommand istrustedCommand))
    ]

mockMessage :: T.Text -> T.Text
mockMessage =
  snd .
  T.mapAccumL
    (\t ->
       (not t, ) .
       if t
         then Data.Char.toUpper
         else Data.Char.toLower)
    True

readCommand :: Read a => CommandHandler (Maybe a) -> CommandHandler T.Text
readCommand = contramapCH (readMaybe . T.unpack)

justCommand :: CommandHandler a -> CommandHandler (Maybe a)
justCommand commandHandler message@Message {messageContent = Just arg} =
  commandHandler $ fmap (const arg) message
justCommand _ message =
  replyMessage $ fmap (const "Could not parse arguments") message

commandArgsCommand :: CommandHandler (Command T.Text) -> CommandHandler T.Text
commandArgsCommand commandHandler message@Message {messageContent = text} =
  case textAsCommand text of
    Just command -> commandHandler $ fmap (const command) message
    Nothing ->
      replyMessage $ fmap (const "Command as an argument is expected") message

voidCommand :: CommandHandler () -> CommandHandler a
voidCommand commandHandler = commandHandler . void

firstArgCommand :: CommandHandler a -> CommandHandler [a]
firstArgCommand _ message@Message {messageContent = []} =
  replyMessage $ fmap (const "Not enough arguments") message
firstArgCommand commandHandler message@Message {messageContent = args:_} =
  commandHandler $ fmap (const args) message

modCommand :: CommandHandler a -> CommandHandler a
modCommand = senderAuthorizedCommand senderAuthority "Only for mods"

senderAuthorizedCommand ::
     (Sender -> Bool) -- sender predicate
  -> T.Text -- unauthorized response
  -> CommandHandler a -- command handler
  -> CommandHandler a
senderAuthorizedCommand predicate unauthorizedResponse commandHandler message =
  if predicate $ messageSender message
    then commandHandler message
    else replyMessage (const unauthorizedResponse <$> message)

authorizeSender ::
     (Sender -> Bool) -> Reaction Message (Maybe a) -> Reaction Message a
authorizeSender p =
  transR
    (\msg ->
       if p $ messageSender msg
         then Just <$> msg
         else const Nothing <$> msg)

pairArgsCommand :: CommandHandler (a, a) -> CommandHandler [a]
pairArgsCommand commandHandler message@Message {messageContent = [x, y]} =
  commandHandler $ fmap (const (x, y)) message
pairArgsCommand _ message@Message {messageContent = args} =
  replyMessage $
  fmap
    (const
       [qms|Expected two arguments
                     but got {length args}|])
    message

regexArgsCommand :: String -> CommandHandler [T.Text] -> CommandHandler T.Text
regexArgsCommand regexString commandHandler Message { messageSender = sender
                                                    , messageContent = args
                                                    } =
  either
    (replyToSender sender . T.pack)
    (commandHandler . Message sender)
    regexArgs
  where
    regexArgs = do
      regex <- compile defaultCompOpt defaultExecOpt regexString
      result <- execute regex stringArgs
      case result of
        Just matches ->
          case map (T.pack . flip extract stringArgs) $ elems matches of
            _:finalArgs -> Right finalArgs
            [] -> Left "Not enough arguments"
        Nothing -> Left [qms|Command doesn't match '{regexString}' regex|]
    stringArgs = T.unpack args

bot :: Bot
bot Join = do
  startPeriodicCommands dispatchCommand
  periodicEffect (60 * 1000) announceRunningPoll
bot event@(Msg sender text) = do
  recordUserMsg sender text
  forbidden <- forbidLinksForPlebs event
  unless forbidden $ do
    runReaction voteMessage $ Message sender text
    mapM redirectAlias (textAsPipe text) >>= dispatchPipe . Message sender

helpCommand :: CommandTable -> CommandHandler T.Text
helpCommand commandTable message@Message {messageContent = ""} =
  replyMessage $ fmap (const [qm|Available commands: {commandList}|]) message
  where
    commandList =
      T.concat $
      intersperse (T.pack ", ") $
      map (\x -> T.concat [T.pack "!", x]) $ M.keys commandTable
helpCommand commandTable Message { messageSender = sender
                                 , messageContent = command
                                 } =
  replyToSender sender $
  maybe "Cannot find such command FeelsBadMan" fst $
  M.lookup command commandTable

dispatchPipe :: Message [Command T.Text] -> Effect ()
dispatchPipe message@Message {messageContent = [command]} =
  dispatchCommand $ fmap (const command) message
dispatchPipe Message {messageContent = []} = return ()
-- TODO(#223): dispatchPipe doesn't support several commands
dispatchPipe _ = return ()

dispatchCommand :: Message (Command T.Text) -> Effect ()
dispatchCommand message = do
  dispatchBuiltinCommand message
  dispatchCustomCommand message

dispatchBuiltinCommand :: Message (Command T.Text) -> Effect ()
dispatchBuiltinCommand message@Message {messageContent = Command { commandName = name
                                                                 , commandArgs = args
                                                                 }} =
  maybe
    (return ())
    (\(_, f) -> runReaction f $ fmap (const args) message)
    (M.lookup name builtinCommands)
