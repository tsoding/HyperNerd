{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import           Bot.Poll
import           Bot.Quote
import           Bot.Replies
import           Command
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Effect
import           Network.HTTP.Simple
import           Text.Printf
import           Bot.Russify

data Event = Join
           | Msg T.Text T.Text

type Bot = Event -> Effect ()

type CommandHandler a = T.Text -> a -> Effect ()
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
                      ]

authorizeCommand :: [T.Text] -> CommandHandler T.Text -> CommandHandler T.Text
authorizeCommand authorizedPeople commandHandler sender args =
    if sender `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser sender $ "You are not authorized to use this command! HyperNyard"

wordsArgsCommand :: CommandHandler [T.Text] -> CommandHandler T.Text
wordsArgsCommand commandHandler sender args =
    commandHandler sender $ T.words args

bot :: Bot
bot Join = say "HyperNyard"
bot (Msg user text) = maybe (return ())
                            (dispatchCommand user)
                            (textAsCommand text)

bttvApiResponseAsEmoteList :: Object -> Either String [T.Text]
bttvApiResponseAsEmoteList =
    parseEither $ \obj ->
        obj .: "emotes" >>= sequence . map (.: "code")

ffzApiResponseAsEmoteList :: Object -> Either String [T.Text]
ffzApiResponseAsEmoteList =
    parseEither $ \obj ->
        do room <- obj .: "room"
           sets <- obj .: "sets"
           setId <- (room .: "set") :: Parser Int
           roomSet <- sets .: (T.pack $ show $ setId)
           emoticons <- roomSet .: "emoticons"
           sequence $ map (.: "name") emoticons

helpCommand :: CommandTable T.Text -> CommandHandler T.Text
helpCommand commandTable sender "" =
    replyToUser sender $
    T.pack $
    printf "Available commands: %s" $
    T.concat $
    intersperse (T.pack ", ") $
    map (\x -> T.concat [T.pack "!", x]) $
    M.keys commandTable
helpCommand commandTable sender command =
    maybe (replyToUser sender "Cannot find your stupid command HyperNyard")
          (replyToUser sender)
          (fst <$> M.lookup command commandTable)

requestEmoteList :: T.Text -> String -> (Object -> Either String [T.Text]) -> Effect ()
requestEmoteList sender url emoteListExtractor =
    maybe (logMsg $ T.pack $ printf "Couldn't parse URL %s" url)
          (\request ->
               do response <- eitherDecode
                              <$> getResponseBody
                              <$> httpRequest request
                  case response >>= emoteListExtractor of
                    Left err -> logMsg
                                $ T.pack
                                $ printf "Couldn't parse Emote List response: %s" err
                    Right emotes -> replyToUser sender
                                    $ T.pack
                                    $ printf "Available emotes: %s"
                                    $ T.concat $ intersperse " "
                                    $ emotes)
          (parseRequest url)

ffzCommand :: CommandHandler T.Text
ffzCommand sender _ = requestEmoteList sender url ffzApiResponseAsEmoteList
    where url = "https://api.frankerfacez.com/v1/room/tsoding"

bttvCommand :: CommandHandler T.Text
bttvCommand sender _ = requestEmoteList sender url bttvApiResponseAsEmoteList
    where url = "https://api.betterttv.net/2/channels/tsoding"

dispatchCommand :: T.Text -> Command T.Text -> Effect ()
dispatchCommand user command =
    maybe (replyToUser user "LUL")
          (\(_, f) -> f user $ commandArgs command)
          (M.lookup (commandName command) commands)
