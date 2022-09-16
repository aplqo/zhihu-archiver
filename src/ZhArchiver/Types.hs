module ZhArchiver.Types (Id, Time, defaultTimeZone, parseTime) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import Data.Time.Clock.System (systemToUTCTime)
import Data.Time.LocalTime

type Id = Int

type Time = ZonedTime

defaultTimeZone :: TimeZone
defaultTimeZone = hoursToTimeZone 8

parseTime :: JSON.Value -> Parser Time
parseTime v = utcToZonedTime defaultTimeZone . systemToUTCTime <$> parseJSON v