{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Bot
  ( Bot
  , bot
  ) where

import Bot.Alias
import Bot.Banwords
import Bot.BotUserInfo
import Bot.BttvFfz
import Bot.CustomCommand
import Bot.Dubtrack
import Bot.Help
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
import Control.Comonad
import Control.Monad
import Data.Array
import Data.Char
import Data.Either
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Text as T
import Effect
import Entity
import Events
import Network.HTTP.Simple
import qualified Network.URI.Encode as URI
import Reaction
import Safe
import Text.InterpolatedString.QM
import Text.Read
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import Bot.Raffle

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
        , cmapR (readMaybe . T.unpack) quoteCommand))
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
    , ("help", ("Send help", helpCommand builtinCommands))
    , ( "poll"
      , ( "Starts a poll. !poll <duration:secs> option1; option2; ...; option3"
        , Reaction $
          modCommand $
          -- TODO(#362): !poll command does not parse negative numbers
          regexArgsCommand "([0-9]+) (.*)" $
          pairArgsCommand $
          contramapCH
            (\(duration, options) ->
               fmap
                 (, filter (not . T.null) $ map T.strip $ T.splitOn ";" options) $
               readMaybe $ T.unpack duration) $
          justCommand pollCommand))
    , ( "cancelpoll"
      , ( "Cancels the current poll"
        , Reaction $ modCommand $ voidCommand cancelPollCommand))
    , ( "checkpoll"
      , ("", Reaction $ modCommand $ voidCommand currentPollCommand))
    , ("uptime", ("Show stream uptime", cmapR (const ()) uptimeCommand))
    , ("rq", ("Get random quote from your log", randomLogRecordCommand))
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
                logMsg [qms|[WARNING] Could not parse arguments: {msg}|]
              Right (n, regex) -> do
                logs <-
                  selectEntities "LogRecord" $
                  Take n $ SortBy "timestamp" Desc All
                traverse_ (banUser . lrUser . entityPayload) $
                  filter
                    (isRight . execute regex . T.unpack . lrMsg . entityPayload)
                    logs))
    , ("cycle", ("Mock the message", cmapR mockMessage $ liftR say ignore))
    , ( "trust"
      , ( "Makes the user trusted"
        , Reaction $
          modCommand $ regexArgsCommand "(.+)" $ firstArgCommand trustCommand))
    , ( "untrust"
      , ( "Untrusts the user"
        , Reaction $
          modCommand $ regexArgsCommand "(.+)" $ firstArgCommand untrustCommand))
    , ( "amitrusted"
      , ("Check if you are a trusted user", cmapR (const ()) amitrustedCommand))
    , ( "istrusted"
      , ( "Check if the user is trusted"
        , regexArgs "(.+)" $
          replyLeft $
          cmapR headMay $ replyOnNothing "Not enough arguments" istrustedCommand))
    , ( "wiggle"
      , ( "Wiggle the tenticle (integration with https://github.com/tsoding/wiggle)"
        , transR (Identity . messageSender) $
          cmapR (URI.encode . T.unpack . senderDisplayName) $
          Reaction $ \(Identity name) -> do
            request <-
              parseRequest [qms|http://localhost:8081/wiggle/{URI.encode name}|]
            void $ httpRequest request))
    , ( "count"
      , ( "Count numbers from 1 to n"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          cmapR (readMaybe . T.unpack) $
          replyOnNothing "Expected number" $
          Reaction $ \w ->
            mapM_ (say . T.pack . show @Int) [1 .. min 20 $ extract w]))
    , ( "wme"
      , ( "Whisper yourself something"
        , Reaction $ \msg ->
            whisperToSender
              (messageSender msg)
              [qms|You asked me to whisper you this: "{messageContent msg}"|]))
    , ( "vanish"
      , ("Timeout yourself for one second", Reaction $ timeoutMessage 1))
    , ( "raffle"
      , ("Start the raffle"
        , authorizeSender senderAuthority $ replyOnNothing "Only for mods" $ cmapR (const 5) raffleCommand))
    , ( "join"
      , ("Join the raffle", joinCommand))
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
    else replyMessage (unauthorizedResponse <$ message)

authorizeSender ::
     (Sender -> Bool) -> Reaction Message (Maybe a) -> Reaction Message a
authorizeSender p =
  transR
    (\msg ->
       if p $ messageSender msg
         then Just <$> msg
         else Nothing <$ msg)

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

regexParseArgs :: T.Text -> T.Text -> Either String [T.Text]
regexParseArgs regexString textArgs = do
  let stringArgs = T.unpack textArgs
  regex <- compile defaultCompOpt defaultExecOpt $ T.unpack regexString
  result <- execute regex stringArgs
  case result of
    Just matches ->
      case map (T.pack . flip Regex.extract stringArgs) $ elems matches of
        _:finalArgs -> Right finalArgs
        [] -> Left "Not enough arguments"
    Nothing -> Left [qms|Command doesn't match '{regexString}' regex|]

regexArgs ::
     Comonad w
  => T.Text
  -> Reaction w (Either String [T.Text])
  -> Reaction w T.Text
regexArgs regexString reaction =
  Reaction $ runReaction reaction . fmap (regexParseArgs regexString)

regexArgsCommand :: String -> CommandHandler [T.Text] -> CommandHandler T.Text
regexArgsCommand regexString commandHandler Message { messageSender = sender
                                                    , messageContent = args
                                                    } =
  either
    (replyToSender sender . T.pack)
    (commandHandler . Message sender)
    parsedArgs
  where
    parsedArgs = do
      regex <- compile defaultCompOpt defaultExecOpt regexString
      result <- execute regex stringArgs
      case result of
        Just matches ->
          case map (T.pack . flip Regex.extract stringArgs) $ elems matches of
            _:finalArgs -> Right finalArgs
            [] -> Left "Not enough arguments"
        Nothing -> Left [qms|Command doesn't match '{regexString}' regex|]
    stringArgs = T.unpack args

mention :: Reaction Message T.Text
mention =
  cmapR T.toUpper $
  liftR
    (\msg ->
       getCompose
         ((, msg) . T.toUpper . buiNickname . entityPayload <$>
          Compose botUserInfo)) $
  ignoreNothing $
  ifR
    (uncurry T.isInfixOf)
    (liftR (const randomMarkov) $
     replyOnNothing "I have nothing to say to you" $ Reaction replyMessage)
    ignore

bot :: Bot
bot (Joined nickname) = do
  updateBotUserInfo nickname
  startPeriodicCommands dispatchCommand
  periodicEffect (60 * 1000) announceRunningPoll
bot event@(Msg sender text) = do
  recordUserMsg sender text
  linkForbidden <- forbidLinksForPlebs event
  banwordsForbidden <- forbidBanwords $ Message sender text
  unless (linkForbidden || banwordsForbidden) $ do
    runReaction voteMessage $ Message sender text
    case textAsPipe text of
      [] -> runReaction mention $ Message sender text
      pipe -> mapM redirectAlias pipe >>= dispatchPipe . Message sender

dispatchRedirect :: Effect () -> Message (Command T.Text) -> Effect ()
dispatchRedirect effect cmd = do
  effectOutput <- T.concat . concatMap (\x -> [" ", x]) <$> listen effect
  dispatchCommand $
    getCompose ((\x -> T.concat [x, effectOutput]) <$> Compose cmd)

-- TODO(#414): there is not cooldown for pipes
dispatchPipe :: Message [Command T.Text] -> Effect ()
dispatchPipe message@Message {messageContent = cmds}
  | length cmds <= cmdsLimit =
    foldl dispatchRedirect (return ()) $ map (\x -> fmap (const x) message) cmds
  | otherwise =
    replyMessage $
    fmap
      (const [qms|The length of the pipe is limited to {cmdsLimit} commands|])
      message
  where
    cmdsLimit = 10

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
