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
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Events
import           Text.InterpolatedString.QM
import           Text.Regex

type Bot = Event -> Effect ()

builtinCommands :: CommandTable T.Text
builtinCommands =
    M.fromList [ ("russify", ("Russify western spy text", russifyCommand))
               , ("addquote", ("Add quote to quote database",
                               senderAuthorizedCommand (\sender -> senderMod sender || senderSubscriber sender)
                                                       "Only subs and mods can add quotes, sorry."
                                                       addQuoteCommand))
               , ("delquote", ("Delete quote from quote database",
                               authorizeCommand [ "tsoding"
                                                , "r3x1m"
                                                ] deleteQuoteCommand))
               , ("quote", ("Get a quote from the quote database", quoteCommand))
               , ("bttv", ("Show all available BTTV emotes", bttvCommand))
               , ("ffz", ("Show all available FFZ emotes", ffzCommand))

               , ("help", ("Send help", helpCommand builtinCommands))
               , ("poll", ("Starts a poll", authorizeCommand [ "tsoding"
                                                             , "r3x1m"
                                                             ]
                                            $ wordsArgsCommand pollCommand))
               , ("vote", ("Vote for a poll option", voteCommand))
               , ("uptime", ("Show stream uptime", uptimeCommand))
               , ("rq", ("Get random quote from your log", randomLogRecordCommand))
               , ("nope", ("Timeout yourself for 1 second", \sender _ -> timeoutSender 1 sender))
               , ("addperiodic", ("Add periodic command", authorizeCommand [ "tsoding"
                                                                           , "r3x1m" ] $
                                                          commandArgsCommand addPeriodicCommand))
               , ("delperiodic", ("Delete periodic command", authorizeCommand [ "tsoding"
                                                                              , "r3x1m" ]
                                                                              removePeriodicCommand))
               , ("addcmd", ("Add custom command", authorizeCommand [ "tsoding"
                                                                    , "r3x1m"
                                                                    ]
                                                     $ regexArgsCommand "([a-zA-Z0-9]+) ?(.*)"
                                                     $ pairArgsCommand
                                                     $ addCustomCommand builtinCommands))
               , ("delcmd", ("Delete custom command", authorizeCommand ["tsoding", "r3x1m"]
                                                        $ deleteCustomCommand builtinCommands))
               , ("updcmd", ("Update custom command", authorizeCommand [ "tsoding"
                                                                       , "r3x1m"
                                                                       ]
                                                        $ regexArgsCommand "([a-zA-Z0-9]+) ?(.*)"
                                                        $ pairArgsCommand
                                                        $ updateCustomCommand builtinCommands))
               , ("song", ("Print currently playing song", noArgsCommand currentSongCommand))
               , ("addalias", ("Add command alias", authorizeCommand [ "tsoding"
                                                                     , "r3x1m"
                                                                     ]
                                                      $ regexArgsCommand "([a-zA-Z0-9]+) ([a-zA-Z0-9]+)"
                                                      $ pairArgsCommand
                                                        addAliasCommand))
               , ("delalias", ("Remove command alias", authorizeCommand [ "tsoding"
                                                                        , "r3x1m"
                                                                        ]
                                                         removeAliasCommand))
               , ("addvar", ("Add variable", authorizeCommand ["tsoding", "r3x1m"] addVariable))
               , ("updvar", ("Update variable", authorizeCommand ["tsoding", "r3x1m"] $
                                                regexArgsCommand "([a-zA-Z0-9]+) ?(.*)" $
                                                pairArgsCommand updateVariable))
               , ("delvar", ("Delete variable", authorizeCommand ["tsoding", "r3x1m"] deleteVariable))
               , ("ban", ("", authorizeCommand ["tsoding", "r3x1m"] $
                              regexArgsCommand "([0-9]+) (.*)" $
                              pairArgsCommand $ \_ (strN, regexStr) ->
                                  do n     <- return $ read $ T.unpack strN
                                     regex <- return $ mkRegex $ T.unpack regexStr
                                     logs  <- selectEntities "LogRecord" $
                                              Take n $
                                              SortBy "timestamp" Desc All
                                     traverse_ (banUser . lrUser . entityPayload) $
                                       filter (isJust . matchRegex regex . T.unpack . lrMsg . entityPayload) logs))
               , ("cycle", ("", \sender -> replyToSender sender . snd . T.mapAccumL (\t -> (not t ,) . if t then Data.Char.toUpper else Data.Char.toLower) True))
               -- TODO(#268): Trust management is not accessible to mods
               , ("trust", ("Makes the user trusted", authorizeCommand ["tsoding", "r3x1m"] trustCommand))
               , ("untrust", ("Untrusts the user", authorizeCommand ["tsoding", "r3x1m"] untrustCommand))
               , ("amitrusted", ("Check if you are a trusted user", noArgsCommand amitrustedCommand))
               , ("istrusted", ("Check if the user is trusted", authorizeCommand ["tsoding", "r3x1m"] $
                                                                regexArgsCommand "(.+)" $
                                                                firstArgCommand
                                                                istrustedCommand))
               ]

commandArgsCommand :: CommandHandler (Command T.Text) -> CommandHandler T.Text
commandArgsCommand commandHandler sender text =
    case textAsCommand text of
      Just command -> commandHandler sender command
      Nothing      -> replyToSender sender "Command as an argument is expected"

noArgsCommand :: CommandHandler () -> CommandHandler a
noArgsCommand commandHandler sender _ =
    commandHandler sender ()

firstArgCommand :: CommandHandler a -> CommandHandler [a]
firstArgCommand _ sender [] = replyToSender sender "Not enough arguments"
firstArgCommand commandHandler sender (arg:_) =
    commandHandler sender arg

authorizeCommand :: [T.Text] -> CommandHandler a -> CommandHandler a
authorizeCommand authorizedPeople commandHandler sender args =
    if senderName sender `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser (senderName sender)
                     "You are not authorized to use this command! HyperNyard"

senderAuthorizedCommand :: (Sender -> Bool) -- sender predicate
                        -> T.Text           -- unauthorized response
                        -> CommandHandler a -- command handler
                        -> CommandHandler a
senderAuthorizedCommand predicate unauthorizedResponse commandHandler sender args =
    if predicate sender
    then commandHandler sender args
    else replyToUser (senderName sender) unauthorizedResponse

pairArgsCommand :: CommandHandler (a, a) -> CommandHandler [a]
pairArgsCommand commandHandler sender [x, y] = commandHandler sender (x, y)
pairArgsCommand _ sender args =
    replyToSender sender [qm|Expected two arguments
                             but got {length args}|]

regexArgsCommand :: String -> CommandHandler [T.Text] -> CommandHandler T.Text
regexArgsCommand regexString commandHandler sender args =
    maybe (replyToSender sender [qms|Command doesn't match
                                     '{regexString}' regex|])
          (commandHandler sender . map T.pack)
      $ matchRegex (mkRegex regexString)
      $ T.unpack args

wordsArgsCommand :: CommandHandler [T.Text] -> CommandHandler T.Text
wordsArgsCommand commandHandler sender args =
    commandHandler sender $ T.words args

bot :: Bot
bot Join = startPeriodicCommands dispatchCommand
bot event@(Msg sender text) = do
  recordUserMsg sender text
  forbidden <- forbidLinksForPlebs event
  unless forbidden $
    mapM redirectAlias (textAsPipe text) >>= dispatchPipe sender

helpCommand :: CommandTable T.Text -> CommandHandler T.Text
helpCommand commandTable sender "" =
    replyToSender sender [qm|Available commands: {commandList}|]
    where commandList = T.concat $
                        intersperse (T.pack ", ") $
                        map (\x -> T.concat [T.pack "!", x]) $
                        M.keys commandTable
helpCommand commandTable sender command =
    replyToSender sender $
    maybe "Cannot find such command FeelsBadMan" fst $
    M.lookup command commandTable

dispatchPipe :: Sender -> [Command T.Text] -> Effect ()
dispatchPipe sender [command] = dispatchCommand sender command
dispatchPipe _ [] = return ()
-- TODO(#223): dispatchPipe doesn't support several commands
dispatchPipe _ _ = return ()

dispatchCommand :: Sender -> Command T.Text -> Effect ()
dispatchCommand sender cmd =
    do dispatchBuiltinCommand sender cmd
       dispatchCustomCommand sender cmd

dispatchBuiltinCommand :: Sender -> Command T.Text -> Effect ()
dispatchBuiltinCommand sender command =
    maybe (return ())
          (\(_, f) -> f sender $ commandArgs command)
          (M.lookup (commandName command) builtinCommands)
