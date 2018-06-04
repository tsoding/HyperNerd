{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..), Sender(..), TwitchStream(..)) where

import           Bot.BttvFfz
import           Bot.Log
import           Bot.Poll
import           Bot.Quote
import           Bot.Replies
import           Bot.Russify
import           Command
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Events
import           Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import           Safe
import           Text.Printf

type Bot = Event -> Effect ()

type CommandHandler a = Sender -> a -> Effect ()
type CommandTable a = M.Map T.Text (T.Text, CommandHandler a)

data TwitchStream = TwitchStream { tsStartedAt :: UTCTime
                                 , tsTitle :: T.Text
                                 }

humanReadableDiffTime :: NominalDiffTime -> T.Text
humanReadableDiffTime t =
    T.pack
      $ unwords
      $ map (\(name, amount) -> printf "%d %s" amount name)
      $ filter ((> 0) . snd) components
    where s :: Int
          s = round t
          components :: [(T.Text, Int)]
          components = [("days" :: T.Text, s `div` secondsInDay),
                        ("hours",   (s `mod` secondsInDay) `div` secondsInHour),
                        ("minutes", ((s `mod` secondsInDay) `mod` secondsInHour) `div` secondsInMinute),
                        ("seconds", ((s `mod` secondsInDay) `mod` secondsInHour) `mod` secondsInMinute)]
          secondsInDay = 24 * secondsInHour
          secondsInHour = 60 * secondsInMinute
          secondsInMinute = 60

twitchStreamsParser :: Object -> Parser [TwitchStream]
twitchStreamsParser obj =
    obj .: "data" >>= mapM (\s ->
        do title     <- s .: "title"
           startedAt <- s .: "started_at"
           return TwitchStream { tsStartedAt = startedAt
                               , tsTitle = title
                               })

twitchStreamByLogin :: T.Text -> Effect (Maybe TwitchStream)
twitchStreamByLogin login =
    do request <- parseRequest
                    $ printf "https://api.twitch.tv/helix/streams?user_login=%s"
                    $ URI.encode
                    $ T.unpack login
       response <- twitchApiRequest request
       payload  <- return $ eitherDecode $ getResponseBody response
       either (errorEff . T.pack)
              (return . listToMaybe)
              (payload >>= parseEither twitchStreamsParser)

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
                      , ("uptime", ("Show stream uptime", \sender _ ->
                                        do channel  <- return
                                                         $ T.pack
                                                         $ fromMaybe "tsoding"
                                                         $ tailMay
                                                         $ T.unpack
                                                         $ senderChannel sender
                                           name     <- return $ senderName sender
                                           response <- twitchStreamByLogin channel
                                           maybe (replyToUser name "Not even streaming LUL")
                                                 (\twitchStream ->
                                                    do streamStartTime <- return $ tsStartedAt twitchStream
                                                       currentTime     <- now
                                                       replyToUser name
                                                         $ T.pack
                                                         $ printf "Streaming for %s"
                                                         $ humanReadableDiffTime
                                                         $ diffUTCTime currentTime streamStartTime)
                                                 response
                                   ))
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

bot :: Bot
bot Join = say "HyperNyard"
bot (Msg sender text) =
    do recordUserMsg sender text
       maybe (return ())
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
