{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Bot
  ( Bot
  , bot
  ) where

import Bot.Alias
import Bot.Banwords
import Bot.BttvFfz
import Bot.Calc
import Bot.CustomCommand
import Bot.Dubtrack
import Bot.Friday
import Bot.Help
import Bot.Links
import Bot.Log
import Bot.Periodic
import Bot.Poll
import Bot.Quote
import Bot.Raffle
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
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Effect
import Entity
import Network.HTTP.Simple (parseRequest)
import qualified Network.URI.Encode as URI
import Reaction
import Safe
import System.Random
import Text.InterpolatedString.QM
import Text.Read
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import Transport

type Bot = InEvent -> Effect ()

tsodingTwitchedDiscordRole :: Role
tsodingTwitchedDiscordRole = DiscordRole 542590649103286273

tsodingTrustedDiscordRole :: Role
tsodingTrustedDiscordRole = DiscordRole 543864981171470346

-- TODO(#549): Authorization errors are not consistent
--   Sometimes they say "Only for mods", sometimes — "Only for roles: <role vector>". It should always be the later.
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
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          -- TODO(#362): !poll command does not parse negative numbers
          regexArgs "([0-9]+) (.*)" $
          replyLeft $
          pairArgs $
          replyLeft $
          cmapR
            (\(duration, options) ->
               fmap
                 (, filter (not . T.null) $ map T.strip $ T.splitOn ";" options) $
               readMaybe $ T.unpack duration) $
          replyOnNothing "Could not parse arguments" pollCommand))
    , ( "cancelpoll"
      , ( "Cancels the current poll"
        , authorizeSender senderAuthority $ transR void cancelPollCommand))
    , ( "checkpoll"
      , ("", authorizeSender senderAuthority $ transR void currentPollCommand))
    , ("uptime", ("Show stream uptime", cmapR (const ()) uptimeCommand))
    , ("rq", ("Get random quote from your log", randomLogRecordCommand))
    , ( "addperiodic"
      , ( "Add periodic command"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          cmapR textAsCommand $
          replyOnNothing "Command as an argument is expected" addPeriodicCommand))
    , ( "delperiodic"
      , ( "Delete periodic command"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" removePeriodicCommand))
    , ( "periodicon"
      , ( "Enable periodic timer"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" enablePeriodicTimerCommand))
    , ( "periodicoff"
      , ( "Disable periodic timer"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" disablePeriodicTimerCommand))
    , ("periodicstat", ("Status of Periodic Timer", statusPeriodicTimerCommand))
    , ( "addcmd"
      , ( "Add custom command"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "([a-zA-Z0-9]+) ?(.*)" $
          replyLeft $ pairArgs $ replyLeft $ addCustomCommand builtinCommands))
    , ( "delcmd"
      , ( "Delete custom command"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $ deleteCustomCommand builtinCommands))
    , ( "updcmd"
      , ( "Update custom command"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "([a-zA-Z0-9]+) ?(.*)" $
          replyLeft $ pairArgs $ replyLeft $ updateCustomCommand builtinCommands))
               -- TODO(#337): use help instead of !showcmd
    , ( "showcmd"
      , ( "Show custom command definition"
        , regexArgs "([a-zA-Z0-9]+)" $
          replyLeft $
          cmapR headMay $
          replyOnNothing "Not enough arguments" $
          showCustomCommand builtinCommands))
    , ( "timescmd"
      , ( "Show amount of times the custom commands was invoked"
        , regexArgs "([a-zA-Z0-9]+)" $
          replyLeft $
          cmapR headMay $
          replyOnNothing "Not enough arguments" $
          timesCustomCommand builtinCommands))
    , ("song", ("Print currently playing song", transR void currentSongCommand))
    , ( "addalias"
      , ( "Add command alias"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "([a-zA-Z0-9]+) ([a-zA-Z0-9]+)" $
          replyLeft $ pairArgs $ replyLeft addAliasCommand))
    , ( "delalias"
      , ( "Remove command alias"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" removeAliasCommand))
    , ( "addvar"
      , ( "Add variable"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" addVariable))
    , ( "updvar"
      , ( "Update variable"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "([a-zA-Z0-9]+) ?(.*)" $
          replyLeft $ pairArgs $ replyLeft updateVariable))
    , ( "delvar"
      , ( "Delete variable"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" deleteVariable))
    , ( "nuke"
      , ( [qms|Looks at N previous messages and bans all of
               the users whose messages match provided regex|]
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "([0-9]+) (.*)" $
          replyLeft $
          pairArgs $
          replyLeft $
          Reaction $ \Message { messageContent = (strN, regexStr)
                              , messageSender = sender
                              } -> do
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
                  selectEntities Proxy $ Take n $ SortBy "timestamp" Desc All
                traverse_
                  (banUser (senderChannel sender) . lrUser . entityPayload) $
                  filter
                    (isRight . execute regex . T.unpack . lrMsg . entityPayload)
                    logs))
    , ("cycle", ("Mock the message", cmapR mockMessage sayMessage))
    , ( "trust"
      , ( "Makes the user trusted"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "(.+)" $
          replyLeft $
          cmapR headMay $ replyOnNothing "Not enough arguments" trustCommand))
    , ( "untrust"
      , ( "Untrusts the user"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $
          regexArgs "(.+)" $
          replyLeft $
          cmapR headMay $ replyOnNothing "Not enough arguments" untrustCommand))
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
    , ( "wme"
      , ( "Whisper yourself something"
        , Reaction $ \msg ->
            whisperToSender
              (messageSender msg)
              [qms|You asked me to whisper you this: "{messageContent msg}"|]))
    , ( "vanish"
      , ("Timeout yourself for one second", Reaction $ timeoutMessage 1))
    , ( "raffle"
      , ( "Start the raffle"
        , authorizeSender senderAuthority $
          replyOnNothing "Only for mods" $ cmapR (const 5) raffleCommand))
    , ("join", ("Join the raffle", joinCommand))
    -- TODO(#562): !friday allows arbitrary text
    , ( "friday"
      , ( "Suggest video for the friday stream"
        , onlyForRoles
            [InternalRole "Trusted", tsodingTrustedDiscordRole]
            fridayCommand))
    , ( "twitch"
      , ( "Send message to Tsoding Twitch channel"
        , onlyForRoles [tsodingTwitchedDiscordRole] $
          liftR (say (TwitchChannel "#tsoding")) ignore))
    , ( "roles"
      , ( "Show your roles"
        , transR duplicate $
          cmapR (T.pack . show . senderRoles . messageSender) $
          Reaction replyMessage))
    , ("markov", ("Generate Markov message", markov))
    , ( "nextvideo"
      , ( "Get the next video for Smart Stream"
        , onlyForRoles authorityRoles $ transR void nextVideoCommand))
    , ("video", ("Print the current video", transR void videoCommand))
    , ( "videocount"
      , ("Print amount of videos in the queue", transR void videoCountCommand))
    , ( "setvideotime"
      , ( "Set the time cursor for the video queue"
        , onlyForRoles authorityRoles $
          cmapR (readMay . T.unpack) $
          replyOnNothing "Cannot parse this as UTCTime" setVideoDateCommand))
    , ("calc", ("Calculator", calcCommand))
    , ("omega", ("OMEGALUL", cmapR (omega 3) sayMessage))
    , ( "localtime"
      , ( "A simple command that show local time in a timezone"
        , cmapR (readMay . T.unpack) $
          replyOnNothing
            [qms|Please provide the number of minutes
                 offset from UTC. Positive means local
                 time will be later in the
                 day than UTC.|] $
          cmapR (return . minutesToTimeZone) $
          liftR (flip (liftM2 utcToLocalTime) now) $
          cmapR (T.pack . show) $ Reaction replyMessage))
    ]

combineDecks :: [a] -> [a] -> [a]
combineDecks xs ys
  | length xs > length ys = combineDecks ys xs
  | otherwise = concat (zs ++ map return (drop (length zs) ys))
  where
    zs = zipWith (\a b -> [a, b]) xs ys

swapDeck :: RandomGen gen => ([a], gen) -> ([a], gen)
swapDeck (xs, g0) = (combineDecks (drop k xs) (take k xs), g1)
  where
    (k, g1) = randomR (0, length xs - 1) g0

shuffle :: RandomGen gen => ([a], gen) -> ([a], gen)
shuffle t = fromMaybe t $ headMay $ drop 100 $ iterate swapDeck t

replaceAt :: Int -> T.Text -> T.Text -> T.Text
replaceAt i rep input = T.concat [left, rep, T.tail right]
  where
    (left, right) = T.splitAt i input

omega :: Int -> T.Text -> T.Text
omega n s =
  foldl' (\acc i -> replaceAt i " OMEGALUL " acc) s $
  sortBy (flip compare) $ take n $ fst $ shuffle (xs, g)
  where
    g = mkStdGen $ sum $ map ord $ T.unpack s
    xs = elemIndices 'O' $ T.unpack $ T.map toUpper s

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

onlyForRoles :: [Role] -> Reaction Message a -> Reaction Message a
onlyForRoles roles reaction =
  transR duplicate $
  ifR
    (any (`elem` roles) . senderRoles . messageSender)
    (cmapR extract reaction)
    (cmapR (const [qms|Only for roles: {roles}|]) $ Reaction replyMessage)

authorizeSender ::
     (Sender -> Bool) -> Reaction Message (Maybe a) -> Reaction Message a
authorizeSender p =
  transR
    (\msg ->
       if p $ messageSender msg
         then Just <$> msg
         else Nothing <$ msg)

pairArgs :: Comonad w => Reaction w (Either String (a, a)) -> Reaction w [a]
pairArgs =
  cmapR $ \case
    [x, y] -> Right (x, y)
    args -> Left [qms|Expected 2 arguments but got {length args}|]

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

markov :: Reaction Message a
markov =
  liftR (const randomMarkov) $
  replyOnNothing "I have nothing to say to you" $ Reaction replyMessage

mention :: Reaction Message a
mention =
  transR
    (\msg ->
       if messageMentioned msg
         then Just <$> msg
         else Nothing <$ msg) $
  ignoreNothing markov

bot :: Bot
bot (Joined channel@(TwitchChannel _)) = do
  startPeriodicCommands channel dispatchCommand
  periodicEffect (60 * 1000) (announceRunningPoll channel)
-- TODO(#550): Periodic commands don't work in Discord channels
bot (Joined channel@(DiscordChannel _)) =
  periodicEffect (60 * 1000) (announceRunningPoll channel)
bot (InMsg msg) =
  runReaction (dupLiftExtractR internalMessageRoles messageReaction) msg

messageReaction :: Reaction Message T.Text
messageReaction =
  Reaction $ \msg@Message { messageContent = text
                          , messageSender = sender
                          , messageMentioned = mentioned
                          } -> do
    recordUserMsg msg
    linkForbidden <- forbidLinksForPlebs msg
    banwordsForbidden <- forbidBanwords msg
    unless (linkForbidden || banwordsForbidden) $ do
      runReaction voteMessage msg
      case textAsPipe text of
        [] -> runReaction mention msg
        pipe ->
          mapM redirectAlias pipe >>= dispatchPipe . Message sender mentioned

dispatchRedirect :: Effect () -> Message (Command T.Text) -> Effect ()
dispatchRedirect effect cmd = do
  effectOutput <- T.concat . concatMap (\x -> [" ", x]) <$> listen effect
  dispatchCommand $
    getCompose ((\x -> T.concat [x, effectOutput]) <$> Compose cmd)

-- TODO(#414): there is not cooldown for pipes
dispatchPipe :: Message [Command T.Text] -> Effect ()
dispatchPipe message@Message { messageSender = Sender {senderRoles = roles}
                             , messageContent = cmds
                             }
  | not (any (`elem` coolRoles) roles) && length cmds > plebPipeLimit =
    replyMessage $
    fmap
      (const
         [qms|The length of the pipe is limited to {plebPipeLimit}.
              Subscribe to increase the limit:
              https://www.twitch.tv/products/tsoding|])
      message
  | any (`elem` coolRoles) roles && length cmds > pipeLimit =
    replyMessage $
    fmap
      (const [qms|The length of the pipe is limited to {pipeLimit} commands|])
      message
  | otherwise =
    foldl dispatchRedirect (return ()) $ map (\x -> fmap (const x) message) cmds
  where
    pipeLimit = 10
    plebPipeLimit = 2
    coolRoles = [tsodingTwitchedDiscordRole, TwitchSub] ++ authorityRoles

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
