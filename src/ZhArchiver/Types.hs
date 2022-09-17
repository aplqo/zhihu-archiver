{-# LANGUAGE TemplateHaskellQuotes #-}

module ZhArchiver.Types
  ( Id,
    Time,
    defaultTimeZone,
    convertTime,
    poTime,
  )
where

import Data.Time.Clock.System
import Data.Time.LocalTime
import ZhArchiver.RawParser.TH

type Id = Int

type Time = ZonedTime

defaultTimeZone :: TimeZone
defaultTimeZone = hoursToTimeZone 8

convertTime :: SystemTime -> Time
convertTime = utcToZonedTime defaultTimeZone . systemToUTCTime

poTime :: ParseOpt
poTime = PoMap [|convertTime|]
