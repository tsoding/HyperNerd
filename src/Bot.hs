{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot (Bot, bot, Event(..), Sender(..), TwitchStream(..)) where

import           Bot.BttvFfz
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
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Events
import           Text.Printf
import           Text.RawString.QQ
import           Text.Regex

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
                                                   $ wordsArgsCommand pollCommand))
                      , ("vote", ("Vote for a poll option", voteCommand))
                      , ("uptime", ("Show stream uptime", uptimeCommand))
                      , ("rq", ("Get random quote from your log", randomLogRecordCommand))
                      , ("scoods", ([r|OMGScoods ☝🏻|], \_ _ -> say [r|OMGScoods ☝🏻|]))
                      , ("schedule", ("Link to the schedule", \_ _ -> say "Hey! Checkout the new schedule thingy! \
                                                                          \https://tsoding.github.io/schedule-beta/ \
                                                                          \For questions/bug reports please file an issue \
                                                                          \https://github.com/tsoding/schedule-beta/issues/new \
                                                                          \Thanks!" ))
                      , ("nope", ("Timeout yourself for 1 second", \sender _ -> say
                                                                                  $ T.pack
                                                                                  $ printf "/timeout %s 1"
                                                                                  $ senderName sender))
                      , ("lit", ("LIT AF", \_ _ -> say [r|😂 👌 💯 🔥 LIT AF|]))
                      , ("addperiodic", ("Add periodic message", authorizeCommand [ "tsoding"
                                                                                  , "r3x1m"
                                                                                  ]
                                                                   $ \sender message -> do addPeriodicMessage sender message
                                                                                           replyToUser (senderName sender)
                                                                                                       "Added the periodic message"))
                      ]

authorizeCommand :: [T.Text] -> CommandHandler T.Text -> CommandHandler T.Text
authorizeCommand authorizedPeople commandHandler sender args =
    if senderName sender `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser (senderName sender)
                     "You are not authorized to use this command! HyperNyard"

wordsArgsCommand :: CommandHandler [T.Text] -> CommandHandler T.Text
wordsArgsCommand commandHandler sender args =
    commandHandler sender $ T.words args

-- TODO(#146): textContainsLink doesn't recognize URLs without schema
textContainsLink :: T.Text -> Bool
textContainsLink t = isJust
                       $ matchRegex (mkRegex "[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&\\/\\/=]*)")
                       $ T.unpack t

senderIsPleb :: Sender -> Bool
senderIsPleb sender = not $ senderSubscriber sender

forbidLinksForPlebs :: Event -> Maybe (Effect())
forbidLinksForPlebs (Msg sender text)
    | textContainsLink text && senderIsPleb sender =
        -- TODO(#147): use CLEARCHAT command instead of /timeout
        return $ do say $ T.pack $ printf "/timeout %s 10" $ senderName sender
                    replyToUser (senderName sender)
                                "Only subs can post links, sorry."
    | otherwise = Nothing
forbidLinksForPlebs _ = Nothing

bot :: Bot
bot Join = startPeriodicMessages
bot event@(Msg sender text) =
    fromMaybe (do recordUserMsg sender text
                  maybe (return ())
                        (dispatchCommand sender)
                        (textAsCommand text))
              (forbidLinksForPlebs event)

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
