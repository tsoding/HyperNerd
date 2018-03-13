{-# LANGUAGE OverloadedStrings #-}
module Bot (Bot, bot, Event(..)) where

import           Command
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity
import           Network.HTTP.Simple
import           Russify
import           Text.Printf
import           Text.Read

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
                      , ("bttv", ("Show all available BTTV emotes", bttvCommand))
                      , ("ffz", ("Show all available FFZ emotes", ffzCommand))
                      , ("quote", ("Get a quote from the quote database", quoteCommand))
                      , ("help", ("Send help", helpCommand commands))
                      , ("poll", ("Starts a poll", authorizeCommand [ "tsoding"
                                                                    , "r3x1m"
                                                                    ] pollCommand))
                      , ("vote", ("Vote for a poll option", voteCommand))
                      ]

authorizeCommand :: [T.Text] -> CommandHandler T.Text -> CommandHandler T.Text
authorizeCommand authorizedPeople commandHandler sender args =
    if sender `elem` authorizedPeople
    then commandHandler sender args
    else replyToUser sender $ "You are not authorized to use this command! HyperNyard"

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

pollCommand :: T.Text -> T.Text -> Effect ()
pollCommand sender _ = replyToUser sender "I don't support that yet"

voteCommand :: T.Text -> T.Text -> Effect ()
voteCommand sender _ = replyToUser sender "I don't support that yet"

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

russifyCommand :: T.Text -> T.Text -> Effect ()
russifyCommand sender westernSpyMsg =
    replyToUser sender
    $ T.pack
    $ printf "%s KKomrade"
    $ russify westernSpyMsg

addQuoteCommand :: T.Text -> T.Text -> Effect ()
addQuoteCommand sender quoteContent =
    (quoteProperties quoteContent sender <$> now)
    >>= createEntity "quote"
    >>= (quoteAddedReply sender . entityId)

quoteCommand :: T.Text -> T.Text -> Effect ()
quoteCommand sender "" =
    do quoteEntity <- getRandomEntity "quote"
       quoteFoundReply sender quoteEntity
quoteCommand sender quoteIdText =
    case readMaybe $ T.unpack $ quoteIdText of
      Nothing -> replyToUser sender "Couldn't find any quotes"
      Just quoteId -> do quoteEntity <- getEntityById "quote" quoteId
                         quoteFoundReply sender quoteEntity

replyToUser :: T.Text -> T.Text -> Effect ()
replyToUser user text = say $ T.pack $ printf "@%s %s" user text

quoteAddedReply :: T.Text -> Int -> Effect ()
quoteAddedReply user quoteId =
    replyToUser user $ T.pack $ printf "Added the quote under the number %d" quoteId

quoteFoundReply :: T.Text -> Maybe Entity -> Effect ()
quoteFoundReply user (Nothing) = replyToUser user "Couldn't find any quotes"
quoteFoundReply user (Just entity) =
    case M.lookup "content" $ entityProperties entity of
      Nothing ->
          do logMsg $ T.pack $ printf "Quote #%d doesn't have the 'content field'" $ entityId entity
             replyToUser user "Couldn't find any quotes, because of some database issues."
      Just (PropertyText content) ->
          replyToUser user $ T.pack $ printf "%s (%d)" content $ entityId entity
      Just _ -> do logMsg $ T.pack $ printf "Quote #%d content is not text" $ entityId entity
                   replyToUser user "Couldn't find any quotes, because of some database issues."


quoteProperties :: T.Text -> T.Text -> UTCTime -> Properties
quoteProperties content quoter timestamp =
    M.fromList [ ("content", PropertyText content)
               , ("quoter", PropertyText quoter)
               , ("timestamp", PropertyUTCTime timestamp)
               ]
