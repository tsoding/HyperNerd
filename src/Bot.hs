{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot (Bot, bot, Event(..), Sender(..), TwitchStream(..)) where

import           Bot.Alias
import           Bot.BttvFfz
import           Bot.CustomCommand
import           Bot.Dubtrack
import           Bot.Links
import           Bot.Log
import           Bot.Periodic
import           Bot.Poll
import           Bot.Quote
import           Bot.Replies
import           Bot.Russify
import           Bot.Twitch
import           Bot.Variable
import           Command
import           Control.Monad
import           Data.Array
import           Data.Char
import           Data.Either
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Text.InterpolatedString.QM
import           Text.Read
import           Text.Regex.Base.RegexLike
import           Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import           Text.Regex.TDFA.String

type Bot = Event -> Effect ()

builtinCommands :: CommandTable T.Text
builtinCommands =
    M.fromList [ ("russify", ("Russify western spy text", russifyCommand))
               , ("addquote", ("Add quote to quote database",
                               senderAuthorizedCommand (\sender -> senderMod sender || senderSubscriber sender)
                                                       "Only subs and mods can add quotes, sorry."
                                                       addQuoteCommand))
               , ("delquote", ("Delete quote from quote database",
                               modCommand $
                               readCommand $
                               justCommand deleteQuoteCommand))
               , ("quote", ("Get a quote from the quote database", readCommand quoteCommand))
               , ("bttv", ("Show all available BTTV emotes", voidCommand bttvCommand))
               , ("ffz", ("Show all available FFZ emotes", voidCommand ffzCommand))

               , ("help", ("Send help", helpCommand builtinCommands))
               , ("poll", ("Starts a poll", modCommand $
                                            regexArgsCommand "([0-9]+) (.*)" $
                                            pairArgsCommand $
                                            contramapCH (\Message { messageSender = sender
                                                                  , messageContent = (duration, options)
                                                                  } ->
                                                           return $
                                                             Message sender $
                                                             fmap (, T.words options) $
                                                             readMaybe $
                                                             T.unpack duration) $
                                            justCommand pollCommand))
               , ("cancelpoll", ("Cancels the current poll",
                                 modCommand $
                                 voidCommand cancelPollCommand))
               , ("checkpoll", ("", modCommand $
                                    voidCommand currentPollCommand))
               , ("vote", ("Vote for a poll option", voteCommand))
               , ("uptime", ("Show stream uptime", voidCommand uptimeCommand))
               , ("rq", ("Get random quote from your log", randomLogRecordCommand))
               , ("nope", ("Timeout yourself for 1 second", timeoutMessage 1))
               , ("addperiodic", ("Add periodic command", modCommand $
                                                          commandArgsCommand addPeriodicCommand))
               , ("delperiodic", ("Delete periodic command", modCommand removePeriodicCommand))
               , ("addcmd", ("Add custom command", modCommand $
                                                   regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
                                                   pairArgsCommand $
                                                   addCustomCommand builtinCommands))
               , ("delcmd", ("Delete custom command", modCommand $
                                                      deleteCustomCommand builtinCommands))
               , ("updcmd", ("Update custom command", modCommand $
                                                      regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
                                                      pairArgsCommand $
                                                      updateCustomCommand builtinCommands))
               , ("song", ("Print currently playing song", voidCommand currentSongCommand))
               , ("addalias", ("Add command alias", modCommand $
                                                    regexArgsCommand "([a-zA-Z0-9]+) ([a-zA-Z0-9]+)" $
                                                    pairArgsCommand addAliasCommand))
               , ("delalias", ("Remove command alias", modCommand removeAliasCommand))
               , ("addvar", ("Add variable", modCommand addVariable))
               , ("updvar", ("Update variable", modCommand $
                                                regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
                                                pairArgsCommand updateVariable))
               , ("delvar", ("Delete variable", modCommand deleteVariable))
               , ("nuke", ([qms|Looks at N previous messages and bans all of
                                the users whose messages match provided regex|],
                           modCommand $
                           regexArgsCommand "([0-9]+) (.*)" $
                           pairArgsCommand $ \Message { messageContent = (strN, regexStr) } ->
                               do let parsedN       = maybe (Left "Could not parse N") Right $
                                                      readMaybe $
                                                      T.unpack strN
                                  let compiledRegex = compile defaultCompOpt defaultExecOpt $
                                                      T.unpack regexStr
                                  case liftM2 (,) parsedN compiledRegex of
                                    Left msg    -> logMsg [qms|[WARNING] Could not parse
                                                               arguments: {msg}|]
                                    Right (n, regex) -> do
                                      logs  <- selectEntities "LogRecord" $
                                               Take n $
                                               SortBy "timestamp" Desc All
                                      traverse_ (banUser . lrUser . entityPayload) $
                                        filter (isRight . execute regex . T.unpack . lrMsg . entityPayload) logs))
               , ("cycle", ("Mock the message", replyMessage . fmap mockMessage))
               , ("trust", ("Makes the user trusted", modCommand $
                                                      regexArgsCommand "(.+)" $
                                                      firstArgCommand trustCommand))
               , ("untrust", ("Untrusts the user", modCommand $
                                                   regexArgsCommand "(.+)" $
                                                   firstArgCommand untrustCommand))
               , ("amitrusted", ("Check if you are a trusted user", voidCommand amitrustedCommand))
               , ("istrusted", ("Check if the user is trusted", regexArgsCommand "(.+)" $
                                                                firstArgCommand istrustedCommand))
               ]

mockMessage :: T.Text -> T.Text
mockMessage =
    snd .
    T.mapAccumL (\t -> (not t ,) . if t
                                   then Data.Char.toUpper
                                   else Data.Char.toLower) True

readCommand :: Read a => CommandHandler (Maybe a) -> CommandHandler T.Text
readCommand commandHandler = commandHandler . fmap (readMaybe . T.unpack)

justCommand :: CommandHandler a -> CommandHandler (Maybe a)
justCommand commandHandler message@Message { messageContent = Just arg } =
    commandHandler $ fmap (const arg) message
justCommand _ message =
    replyMessage $ fmap (const "Could not parse arguments") message

commandArgsCommand :: CommandHandler (Command T.Text) -> CommandHandler T.Text
commandArgsCommand commandHandler message@Message { messageContent = text } =
    case textAsCommand text of
      Just command -> commandHandler $
                      fmap (const command) message
      Nothing      -> replyMessage $
                      fmap (const "Command as an argument is expected") message

voidCommand :: CommandHandler () -> CommandHandler a
voidCommand commandHandler =
    commandHandler . void

firstArgCommand :: CommandHandler a -> CommandHandler [a]
firstArgCommand _ message@Message { messageContent = [] } =
    replyMessage $ fmap (const "Not enough arguments") message
firstArgCommand commandHandler message@Message { messageContent = args:_ } =
    commandHandler $ fmap (const args) message

modCommand :: CommandHandler a -> CommandHandler a
modCommand = senderAuthorizedCommand senderAuthority "Only for mods"

senderAuthorizedCommand :: (Sender -> Bool) -- sender predicate
                        -> T.Text           -- unauthorized response
                        -> CommandHandler a -- command handler
                        -> CommandHandler a
senderAuthorizedCommand predicate unauthorizedResponse commandHandler message =
    if predicate $ messageSender message
    then commandHandler message
    else replyMessage (const unauthorizedResponse <$> message)

pairArgsCommand :: CommandHandler (a, a) -> CommandHandler [a]
pairArgsCommand commandHandler message@Message { messageContent = [x, y] } =
    commandHandler $
    fmap (const (x, y)) message
pairArgsCommand _ message@Message { messageContent = args } =
    replyMessage $
    fmap (const [qms|Expected two arguments
                     but got {length args}|]) message

regexArgsCommand :: String -> CommandHandler [T.Text] -> CommandHandler T.Text
regexArgsCommand regexString commandHandler Message { messageSender = sender
                                                    , messageContent = args
                                                    } =
    either (replyToSender sender . T.pack) (commandHandler . Message sender) regexArgs
    where regexArgs = do
            regex  <- compile defaultCompOpt defaultExecOpt regexString
            result <- execute regex stringArgs
            case result of
              Just matches -> case map (T.pack . flip extract stringArgs) $ elems matches of
                                _:finalArgs -> Right finalArgs
                                []          -> Left "Not enough arguments"
              Nothing      -> Left [qms|Command doesn't match '{regexString}' regex|]
          stringArgs = T.unpack args

bot :: Bot
bot Join = do
  startPeriodicCommands dispatchCommand
  periodicEffect (60 * 1000) announceRunningPoll
bot event@(Msg sender text) = do
  recordUserMsg sender text
  forbidden <- forbidLinksForPlebs event
  unless forbidden $
    mapM redirectAlias (textAsPipe text) >>= dispatchPipe . Message sender

helpCommand :: CommandTable T.Text -> CommandHandler T.Text
helpCommand commandTable message@Message { messageContent = "" } =
    replyMessage $
    fmap (const [qm|Available commands: {commandList}|]) message
    where commandList = T.concat $
                        intersperse (T.pack ", ") $
                        map (\x -> T.concat [T.pack "!", x]) $
                        M.keys commandTable
helpCommand commandTable Message { messageSender = sender
                                 , messageContent = command
                                 } =
    replyToSender sender $
    maybe "Cannot find such command FeelsBadMan" fst $
    M.lookup command commandTable

dispatchPipe :: Message [Command T.Text] -> Effect ()
dispatchPipe message@Message { messageContent = [command] } =
    dispatchCommand $ fmap (const command) message
dispatchPipe Message { messageContent = [] } = return ()
-- TODO(#223): dispatchPipe doesn't support several commands
dispatchPipe _ = return ()

dispatchCommand :: Message (Command T.Text) -> Effect ()
dispatchCommand message =
    do dispatchBuiltinCommand message
       dispatchCustomCommand message

dispatchBuiltinCommand :: Message (Command T.Text) -> Effect ()
dispatchBuiltinCommand message@Message { messageContent =
                                             Command { commandName = name
                                                     , commandArgs = args
                                                     }
                                       } =
    maybe (return ())
          (\(_, f) -> f $ fmap (const args) message)
          (M.lookup name builtinCommands)
