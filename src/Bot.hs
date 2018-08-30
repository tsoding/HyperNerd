{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..), Sender(..), TwitchStream(..)) where

import           Bot.Alias
import           Bot.BttvFfz
import           Bot.CustomCommand
import           Bot.Dubtrack
import           Bot.Log
import           Bot.Periodic
import           Bot.Poll
import           Bot.Quote
import           Bot.Replies
import           Bot.Russify
import           Bot.Twitch
import           Command
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Events
import           Text.Printf
import           Text.Regex

type Bot = Event -> Effect ()

builtinCommands :: CommandTable T.Text
builtinCommands =
    M.fromList [ ("russify", ("Russify western spy text", russifyCommand))
               , ("addquote", ("Add quote to quote database",
                               authorizeCommand [ "tsoding"
                                                , "r3x1m"
                                                , "bpaf"
                                                , "voldyman"
                                                ] addQuoteCommand))
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
               , ("nope", ("Timeout yourself for 1 second", \sender _ -> say
                                                                           $ T.pack
                                                                           $ printf "/timeout %s 1"
                                                                           $ senderName sender))
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
               ]

commandArgsCommand :: CommandHandler (Command T.Text) -> CommandHandler T.Text
commandArgsCommand commandHandler sender text =
    case textAsCommand text of
      Just command -> commandHandler sender command
      Nothing      -> replyToSender sender "Command as an argument is expected"

noArgsCommand :: CommandHandler () -> CommandHandler a
noArgsCommand commandHandler sender _ =
    commandHandler sender ()

authorizeCommand :: [T.Text] -> CommandHandler a -> CommandHandler a
authorizeCommand authorizedPeople commandHandler sender args =
    if senderName sender `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser (senderName sender)
                     "You are not authorized to use this command! HyperNyard"

pairArgsCommand :: CommandHandler (a, a) -> CommandHandler [a]
pairArgsCommand commandHandler sender [x, y] = commandHandler sender (x, y)
pairArgsCommand _ sender args =
    replyToUser (senderName sender)
      $ T.pack
      $ printf "Expected two arguments but got %d"
      $ length args

regexArgsCommand :: String -> CommandHandler [T.Text] -> CommandHandler T.Text
regexArgsCommand regexString commandHandler sender args =
    maybe (replyToUser (senderName sender)
             $ T.pack
             $ printf "Command doesn't match '%s' regex" regexString)
          (commandHandler sender . map T.pack)
      $ matchRegex (mkRegex regexString)
      $ T.unpack args

wordsArgsCommand :: CommandHandler [T.Text] -> CommandHandler T.Text
wordsArgsCommand commandHandler sender args =
    commandHandler sender $ T.words args

bot :: Bot
bot Join = startPeriodicCommands dispatchCommand
bot (Msg sender text) =
    do recordUserMsg sender text
       mapM redirectAlias (textAsPipe text) >>= dispatchPipe sender

helpCommand :: CommandTable T.Text -> CommandHandler T.Text
helpCommand commandTable sender "" =
    replyToUser (senderName sender)
      $ T.pack
      $ printf "Available commands: %s"
      $ T.concat
      $ intersperse (T.pack ", ")
      $ map (\x -> T.concat [T.pack "!", x])
      $ M.keys commandTable
helpCommand commandTable sender command =
    maybe (replyToUser (senderName sender) "Cannot find your stupid command HyperNyard")
          (replyToUser (senderName sender))
          (fst <$> M.lookup command commandTable)

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
