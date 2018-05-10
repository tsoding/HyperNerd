{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..), Sender(..)) where

import           Bot.BttvFfz
import           Bot.Poll
import           Bot.Quote
import           Bot.Replies
import           Bot.Russify
import           Command
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Events
import           Text.Printf

type Bot = Event -> Effect ()

type CommandHandler a = Sender -> a -> Effect ()
type CommandTable a = M.Map T.Text (T.Text, CommandHandler a)

commands :: CommandTable T.Text
commands = M.fromList [ ("russify", ("Russify western spy text", russifyCommand))
                      , ("addquote", ("Add quote to quote database",
                                      authorizeCommand [ "tsoding"
                                                       , "r3x1m"
                                                       , "bpaf"
                                                       , "voldyman"
                                                       ] addQuoteCommand))
                      , ("quote", ("Get a quote from the quote database", quoteCommand))
                      , ("bttv", ("Show all available BTTV emotes", bttvCommand))
                      , ("ffz", ("Show all available FFZ emotes", ffzCommand))

                      , ("help", ("Send help", helpCommand commands))
                      , ("poll", ("Starts a poll", authorizeCommand [ "tsoding"
                                                                    , "r3x1m"
                                                                    ]
                                                   $ wordsArgsCommand
                                                   $ pollCommand))
                      , ("vote", ("Vote for a poll option", voteCommand))
                      , ("uptime", ("Show stream uptime", \_ _ -> return ()))
                      ]

authorizeCommand :: [T.Text] -> CommandHandler T.Text -> CommandHandler T.Text
authorizeCommand authorizedPeople commandHandler sender args =
    if (senderName sender) `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser (senderName sender) $ "You are not authorized to use this command! HyperNyard"

wordsArgsCommand :: CommandHandler [T.Text] -> CommandHandler T.Text
wordsArgsCommand commandHandler sender args =
    commandHandler sender $ T.words args

bot :: Bot
bot Join = say "HyperNyard"
bot (Msg sender text) = maybe (return ())
                              (dispatchCommand sender)
                              (textAsCommand text)

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

dispatchCommand :: Sender -> Command T.Text -> Effect ()
dispatchCommand sender command =
    maybe (replyToUser (senderName sender) "LUL")
          (\(_, f) -> f sender $ commandArgs command)
          (M.lookup (commandName command) commands)
