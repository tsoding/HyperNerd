{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot
  ( Bot
  , bot
  ) where

import Bot.Alias
import Bot.BttvFfz
import Bot.Calc
import Bot.CustomCommand
import Bot.DocLoc
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
import Data.Char
import Data.Either
import Data.Either.Extra
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
import Regexp
import Safe
import System.Random
import Text.InterpolatedString.QM
import Text.Read
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import Transport

type Bot = InEvent -> Effect ()

tsodingTwitchedDiscordRole :: Role
tsodingTwitchedDiscordRole = DiscordRole 542590649103286273

tsodingTrustedDiscordRole :: Role
tsodingTrustedDiscordRole = DiscordRole 543864981171470346

builtinCommands :: CommandTable
builtinCommands =
  M.fromList
    [ ( "russify"
      , mkBuiltinCommand
          ("Russify western spy text", $githubLinkLocationStr, russifyCommand))
    , ( "addquote"
      , mkBuiltinCommand
          ( "Add quote to quote database"
          , $githubLinkLocationStr
          , authorizeSender
              (\sender -> senderMod sender || senderSubscriber sender) $
            replyOnNothing
              "Only subs and mods can add quotes, sorry."
              addQuoteCommand))
    , ( "delquote"
      , mkBuiltinCommand
          ( "Delete quote from quote database"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            cmapR (readMaybe . T.unpack) $
            replyOnNothing "Expected integer as an argument" deleteQuoteCommand))
    , ( "quote"
      , mkBuiltinCommand
          ( "Get a quote from the quote database"
          , $githubLinkLocationStr
          , cmapR (readMaybe . T.unpack) quoteCommand))
    , ( "bttv"
      , mkBuiltinCommand
          ( "Show all available BTTV emotes"
          , $githubLinkLocationStr
          , cmapR (const ()) bttvCommand))
    , ( "ffz"
      , mkBuiltinCommand
          ( "Show all available FFZ emotes"
          , $githubLinkLocationStr
          , cmapR (const ()) ffzCommand))
    , ( "updateffz"
      , mkBuiltinCommand
          ( "Update FFZ cache"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            cmapR (const ()) updateFfzEmotesCommand))
    , ( "updatebttv"
      , mkBuiltinCommand
          ( "Update BTTV cache"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            cmapR (const ()) updateBttvEmotesCommand))
    , ( "help"
      , mkBuiltinCommand
          ("Send help", $githubLinkLocationStr, helpCommand builtinCommands))
    , ( "poll"
      , mkBuiltinCommand
          ( "Starts a poll. !poll <duration:secs> option1; option2; ...; option3"
          , $githubLinkLocationStr
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
                   (
                   , filter (not . T.null) $ map T.strip $ T.splitOn ";" options) $
                 readMaybe $ T.unpack duration) $
            replyOnNothing "Could not parse arguments" pollCommand))
    , ( "cancelpoll"
      , mkBuiltinCommand
          ( "Cancels the current poll"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $ transR void cancelPollCommand))
    , ( "checkpoll"
      , mkBuiltinCommand
          ( ""
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $ transR void currentPollCommand))
    , ( "uptime"
      , mkBuiltinCommand
          ( "Show stream uptime"
          , $githubLinkLocationStr
          , cmapR (const ()) uptimeCommand))
    , ( "rq"
      , mkBuiltinCommand
          ( "Get random quote from your log"
          , $githubLinkLocationStr
          , randomLogRecordCommand))
    , ( "addperiodic"
      , mkBuiltinCommand
          ( "Add periodic command"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            cmapR textAsCommand $
            replyOnNothing
              "Command as an argument is expected"
              addPeriodicCommand))
    , ( "delperiodic"
      , mkBuiltinCommand
          ( "Delete periodic command"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" removePeriodicCommand))
    , ( "periodicon"
      , mkBuiltinCommand
          ( "Enable periodic timer"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" enablePeriodicTimerCommand))
    , ( "periodicoff"
      , mkBuiltinCommand
          ( "Disable periodic timer"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" disablePeriodicTimerCommand))
    , ( "periodicstat"
      , mkBuiltinCommand
          ( "Status of Periodic Timer"
          , $githubLinkLocationStr
          , statusPeriodicTimerCommand))
    , ( "addcmd"
      , mkBuiltinCommand
          ( "Add custom command"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "([a-zA-Z0-9]+) ?(.*)" $
            replyLeft $ pairArgs $ replyLeft $ addCustomCommand builtinCommands))
    , ( "delcmd"
      , mkBuiltinCommand
          ( "Delete custom command"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $ deleteCustomCommand builtinCommands))
    , ( "updcmd"
      , mkBuiltinCommand
          ( "Update custom command"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "([a-zA-Z0-9]+) ?(.*)" $
            replyLeft $
            pairArgs $ replyLeft $ updateCustomCommand builtinCommands))
               -- TODO(#337): use help instead of !showcmd
    , ( "showcmd"
      , mkBuiltinCommand
          ( "Show custom command definition"
          , $githubLinkLocationStr
          , regexArgs "([a-zA-Z0-9]+)" $
            replyLeft $
            cmapR headMay $
            replyOnNothing "Not enough arguments" $
            showCustomCommand builtinCommands))
    , ( "timescmd"
      , mkBuiltinCommand
          ( "Show amount of times the custom commands was invoked"
          , $githubLinkLocationStr
          , regexArgs "([a-zA-Z0-9]+)" $
            replyLeft $
            cmapR headMay $
            replyOnNothing "Not enough arguments" $
            timesCustomCommand builtinCommands))
    , ( "song"
      , mkBuiltinCommand
          ( "Print currently playing song"
          , $githubLinkLocationStr
          , transR void currentSongCommand))
    , ( "addalias"
      , mkBuiltinCommand
          ( "Add command alias"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "([a-zA-Z0-9]+) ([a-zA-Z0-9]+)" $
            replyLeft $ pairArgs $ replyLeft addAliasCommand))
    , ( "delalias"
      , mkBuiltinCommand
          ( "Remove command alias"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" removeAliasCommand))
    , ( "addvar"
      , mkBuiltinCommand
          ( "Add variable"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" addVariable))
    , ( "updvar"
      , mkBuiltinCommand
          ( "Update variable"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "([a-zA-Z0-9]+) ?(.*)" $
            replyLeft $ pairArgs $ replyLeft updateVariable))
    , ( "delvar"
      , mkBuiltinCommand
          ( "Delete variable"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" deleteVariable))
    , ( "nuke"
      , mkBuiltinCommand
          ( [qms|Looks at N previous messages and bans all of
               the users whose messages match provided regex|]
          , $githubLinkLocationStr
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
                      (isRight .
                       execute regex . T.unpack . lrMsg . entityPayload)
                      logs))
    , ( "cycle"
      , mkBuiltinCommand
          ( "Mock the message"
          , $githubLinkLocationStr
          , cmapR mockMessage sayMessage))
    , ( "trust"
      , mkBuiltinCommand
          ( "Makes the user trusted"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "(.+)" $
            replyLeft $
            cmapR headMay $ replyOnNothing "Not enough arguments" trustCommand))
    , ( "untrust"
      , mkBuiltinCommand
          ( "Untrusts the user"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $
            regexArgs "(.+)" $
            replyLeft $
            cmapR headMay $ replyOnNothing "Not enough arguments" untrustCommand))
    , ( "amitrusted"
      , mkBuiltinCommand
          ( "Check if you are a trusted user"
          , $githubLinkLocationStr
          , cmapR (const ()) amitrustedCommand))
    , ( "istrusted"
      , mkBuiltinCommand
          ( "Check if the user is trusted"
          , $githubLinkLocationStr
          , regexArgs "(.+)" $
            replyLeft $
            cmapR headMay $
            replyOnNothing "Not enough arguments" istrustedCommand))
    , ( "wiggle"
      , mkBuiltinCommand
          ( "Wiggle the tenticle (integration with https://github.com/tsoding/wiggle)"
          , $githubLinkLocationStr
          , transR (Identity . messageSender) $
            cmapR (URI.encode . T.unpack . senderDisplayName) $
            Reaction $ \(Identity name) -> do
              request <-
                parseRequest
                  [qms|http://localhost:8081/wiggle/{URI.encode name}|]
              void $ httpRequest request))
    , ( "wme"
      , mkBuiltinCommand
          ( "Whisper yourself something"
          , $githubLinkLocationStr
          , Reaction $ \msg ->
              whisperToSender
                (messageSender msg)
                [qms|You asked me to whisper you this: "{messageContent msg}"|]))
    , ( "vanish"
      , mkBuiltinCommand
          ( "Timeout yourself for one second"
          , $githubLinkLocationStr
          , Reaction $ timeoutMessage 1))
    , ( "raffle"
      , mkBuiltinCommand
          ( "Start the raffle"
          , $githubLinkLocationStr
          , authorizeSender senderAuthority $
            replyOnNothing "Only for mods" $ cmapR (const 5) raffleCommand))
    , ( "join"
      , mkBuiltinCommand
          ("Join the raffle", $githubLinkLocationStr, joinCommand))
    , ( "friday"
      , mkBuiltinCommand
          ( "Suggest video for the friday stream"
          , $githubLinkLocationStr
          , onlyForRoles
              [InternalRole "Trusted", tsodingTrustedDiscordRole]
              fridayCommand))
    , ( "videoq"
      , mkBuiltinCommand
          ( "Get the link to the current Friday Queue"
          , $githubLinkLocationStr
          , videoQueueCommand))
    , ( "twitch"
      , mkBuiltinCommand
          ( "Send message to Tsoding Twitch channel"
          , $githubLinkLocationStr
          , onlyForRoles [tsodingTwitchedDiscordRole] $
            liftR (say (TwitchChannel "#tsoding")) ignore))
    , ( "roles"
      , mkBuiltinCommand
          ( "Show your roles"
          , $githubLinkLocationStr
          , transR duplicate $
            cmapR (T.pack . show . senderRoles . messageSender) $
            Reaction replyMessage))
    , ( "markov"
      , mkBuiltinCommand
          ("Generate Markov message", $githubLinkLocationStr, markov))
    , ( "nextvideo"
      , mkBuiltinCommand
          ( "Get the next video for Smart Stream"
          , $githubLinkLocationStr
          , onlyForRoles authorityRoles $ transR void nextVideoCommand))
    , ( "video"
      , mkBuiltinCommand
          ( "Print the current video"
          , $githubLinkLocationStr
          , transR void videoCommand))
    , ( "videocount"
      , mkBuiltinCommand
          ( "Print amount of videos in the queue"
          , $githubLinkLocationStr
          , transR void videoCountCommand))
    , ( "setvideotime"
      , mkBuiltinCommand
          ( "Set the time cursor for the video queue"
          , $githubLinkLocationStr
          , onlyForRoles authorityRoles $
            cmapR (readMay . T.unpack) $
            replyOnNothing "Cannot parse this as UTCTime" setVideoDateCommand))
    , ( "calc"
      , mkBuiltinCommand
          ( [qms|Calculator. Supported operation:
                 {T.intercalate ", " $ supportedOps}|]
          , $githubLinkLocationStr
          , calcCommand))
    , ( "omega"
      , mkBuiltinCommand
          ("OMEGALUL", $githubLinkLocationStr, cmapR (omega 3) sayMessage))
    , ( "localtime"
      , mkBuiltinCommand
          ( "A simple command that show local time in a timezone"
          , $githubLinkLocationStr
          , cmapR nameToTimeZone $
            replyLeft $
            cmapR return $
            liftR (flip (liftM2 utcToLocalTime) now) $
            cmapR (T.pack . show) $ Reaction replyMessage))
    , ( "urlencode"
      , mkBuiltinCommand
          ( "!google URL encode"
          , $githubLinkLocationStr
          , liftR (callFun "urlencode" . return) $ ignoreNothing sayMessage))
    , ( "reloadmarkov"
      , mkBuiltinCommand
          ( "Reloads Markov model file"
          , $githubLinkLocationStr
          , onlyForRoles authorityRoles $
            liftR (const reloadMarkov) $
            replyOnNothing "Nothing to reload" $ Reaction replyMessage))
    , ( "config"
      , mkBuiltinCommand
          ( "Bot configuration command"
          , $githubLinkLocationStr
          , onlyForRoles authorityRoles $
            subcommand
              [ ( "help"
                , subcommand
                    [("setgist", setHelpGistId), ("refresh", refreshHelpGistId)])
              ]))
    ]

signText :: T.Text -> Either String Int
signText "-" = Right (-1)
signText "+" = Right 1
signText _ = Left "Could parse the sign"

nameToTimeZone :: T.Text -> Either String TimeZone
nameToTimeZone text = do
  groups <- regexParseArgs "UTC([+-])([0-9]{2})(:([0-9]{2}))?" text
  case groups of
    [sign, hours, "", ""] -> do
      h <- maybeToEither badZoneDesignatorError $ readMay $ T.unpack hours
      s <- signText sign
      return $ minutesToTimeZone (h * 60 * s)
    [sign, hours, _, minutes] -> do
      h <- maybeToEither badZoneDesignatorError $ readMay $ T.unpack hours
      m <- maybeToEither badZoneDesignatorError $ readMay $ T.unpack minutes
      s <- signText sign
      return $ minutesToTimeZone (h * 60 * s + m)
    _ -> Left badZoneDesignatorError
  where
    badZoneDesignatorError =
      "Please provide time zone designator (Examples: UTC-07, UTC+08:35)"

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
bot Started = do
  startRefreshFridayGistTimer
  startRefreshHelpGistTimer builtinCommands
-- TODO(#656): Restarted Twitch transport thread can duplicate timers
bot (Joined channel@(TwitchChannel _)) = do
  startPeriodicCommands channel dispatchCommand
  periodicEffect (60 * 1000) (Just channel) (announceRunningPoll channel)
-- TODO(#550): Periodic commands don't work in Discord channels
bot (Joined channel@(DiscordChannel _)) =
  periodicEffect (60 * 1000) (Just channel) (announceRunningPoll channel)
bot (InMsg msg) =
  runReaction
    (dupLiftExtractR internalMessageRoles $
     longMessageFilter $ linkFilter messageReaction)
    msg

longMessageFilter :: Reaction Message T.Text -> Reaction Message T.Text
longMessageFilter reaction =
  Reaction
    (\case
       msg@Message { messageContent = message
                   , messageSender = sender@Sender { senderRoles = []
                                                   , senderChannel = TwitchChannel _
                                                   }
                   }
         | T.length message > messageLimit -> do
           timeoutSender (T.length message - messageLimit) sender
           replyMessage
             ([qms|Message limit is {messageLimit} characters
                   for untrusted users, sorry.|] <$
              msg)
       msg -> runReaction reaction msg)
  where
    messageLimit = 200

messageReaction :: Reaction Message T.Text
messageReaction =
  Reaction $ \msg@Message { messageContent = text
                          , messageSender = sender
                          , messageMentioned = mentioned
                          } -> do
    recordUserMsg msg
    runReaction voteMessage msg
    case textAsPipe text of
      [] -> runReaction mention msg
      pipe ->
        mapM redirectAlias pipe >>= dispatchPipe . Message sender mentioned

dispatchRedirect :: Effect () -> Message (Command T.Text) -> Effect ()
dispatchRedirect effect cmd = do
  effectOutput <-
    T.strip . T.concat . concatMap (\x -> [" ", x]) <$> listen effect
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
    (\bc -> runReaction (bcReaction bc) $ fmap (const args) message)
    (M.lookup name builtinCommands)
