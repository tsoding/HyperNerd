module Schedule where

import Data.Aeson
import Data.Aeson.Types
import Data.Time
import qualified Data.Text as T
import Data.Time.LocalTime (TimeZone)
import qualified Data.Map as M

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Enum, Show)

newtype ScheduleTimeZone =
  ScheduleTimeZone TimeZone
  deriving (Show)

newtype ScheduleDiffTime =
  ScheduleDiffTime DiffTime
  deriving (Show)

data Project = Project
  { projectName :: T.Text
  , projectDescription :: T.Text
  , projectUrl :: T.Text
  , projectDays :: [DayOfWeek]
  , projectTime :: ScheduleDiffTime
  , projectChannel :: T.Text
  , projectStarts :: Maybe Day
  , projectEnds :: Maybe Day
  } deriving (Show)

data Event = Event { eventDate :: Day
                   , eventTime :: ScheduleDiffTime
                   , eventTitle :: T.Text
                   , eventDescription :: T.Text
                   , eventUrl :: T.Text
                   , eventChannel :: T.Text
                   } deriving Show

newtype EventId =
  EventId Int
  deriving (Eq, Ord, Show)

data EventPatch = EventPatch
  { eventPatchTitle :: Maybe T.Text
  , eventPatchDescription :: Maybe T.Text
  , eventPatchUrl :: Maybe T.Text
  , eventPatchChannel :: Maybe T.Text
  } deriving Show

data Schedule = Schedule
  { scheduleProject :: [Project]
  , scheduleExtraEvents :: [Event]
  , scheduleCancelledEvents :: [EventId]
  , scheduleTimezone :: ScheduleTimeZone
  , schedulePatches :: M.Map EventId EventPatch
  } deriving Show

instance FromJSON Schedule where
  parseJSON (Object v) =
    Schedule <$> v .: "projects" <*> v .: "extraEvents" <*>
    v .: "cancelledEvents" <*>
    v .: "timezone" <*>
    v .: "eventPatches"
  parseJSON invalid = typeMismatch "Schedule" invalid

instance FromJSON Project where
  parseJSON (Object v) =
    Project <$> v .: "name" <*> v .: "description" <*> v .: "url" <*>
    v .: "days" <*>
    v .: "time" <*>
    v .: "channel" <*>
    v .:? "starts" <*>
    v .:? "ends"
  parseJSON invalid = typeMismatch "Project" invalid

instance FromJSON Event where
  parseJSON (Object v) =
    Event <$> v .: "date" <*> v .: "time" <*> v .: "title" <*>
    v .: "description" <*>
    v .: "url" <*>
    v .: "channel"
  parseJSON invalid = typeMismatch "Event" invalid

instance FromJSON EventId where
  parseJSON = fmap EventId . parseJSON

instance FromJSONKey EventId where
  fromJSONKey = EventId <$> fromJSONKey

parseTimeZone :: T.Text -> Parser TimeZone
parseTimeZone "Asia/Novosibirsk" = return $ minutesToTimeZone 420
parseTimeZone s = fail ("Unknown timezone: " ++ T.unpack s)

instance FromJSON ScheduleTimeZone where
  parseJSON (String s) = ScheduleTimeZone <$> parseTimeZone s
  parseJSON invalid = typeMismatch "ScheduleTimeZone" invalid

instance FromJSON EventPatch where
  parseJSON (Object v) =
    EventPatch <$> v .:? "title" <*> v .:? "description" <*> v .:? "url" <*>
    v .:? "channel"
  parseJSON invalid = typeMismatch "EventPatch" invalid

instance FromJSON DayOfWeek where
  parseJSON = fmap (toEnum . (\x -> x - 1)) . parseJSON

parseDiffTime :: T.Text -> Parser DiffTime
parseDiffTime s =
  utctDayTime <$> parseTimeM True defaultTimeLocale "%H:%M" (T.unpack s)

instance FromJSON ScheduleDiffTime where
  parseJSON (String s) = ScheduleDiffTime <$> parseDiffTime s
  parseJSON invalid = typeMismatch "ScheduleDiffTime" invalid
