module ZhArchiver.Types (Id, defaultTimeZone) where

import Data.Time.LocalTime

type Id = Int

defaultTimeZone :: TimeZone
defaultTimeZone = hoursToTimeZone 8