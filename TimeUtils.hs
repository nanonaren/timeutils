{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TimeUtils
   (
     ISO8601 (..)
   , UnixTime
   , secondsFromEpoch
   , utcToUnixTime
   , unixTimeToUTC
   , localTimeToUnixTime
   ) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime

newtype UnixTime = UnixTime{ secondsFromEpoch :: Int }
  deriving (Eq,Ord,Enum)
instance Show UnixTime where
  show = show . secondsFromEpoch

class ISO8601 a where
  showISO8601 :: a -> String
  readISO8601 :: String -> Maybe a

instance ISO8601 Day where
  showISO8601 = formatTime defaultTimeLocale "%Y-%m-%d"
  readISO8601 = parseTimeM False defaultTimeLocale "%Y-%m-%d"

instance ISO8601 UTCTime where
  showISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
  readISO8601 = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

instance ISO8601 UnixTime where
  showISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . unixTimeToUTC
  readISO8601 = fmap utcToUnixTime . parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

instance ISO8601 LocalTime where
  showISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
  readISO8601 = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

{- |
This is lossy because precision of UnixTime is seconds
while the precision of UTCTime is picoseconds.

@
utcToUnixTime . unixTimeToUTC == id
unixTimeToUTC . utcToUnixTime /= id
@
-}
utcToUnixTime :: UTCTime -> UnixTime
utcToUnixTime t = UnixTime $
  dayToSecs (utctDay t) + truncate (utctDayTime t) - startOfTimeSecs
{-# INLINE utcToUnixTime #-}

{- |
This is lossy because precision of UnixTime is seconds
while the precision of LocalTime is picoseconds.

@
localTimeToUnixTime . unixTimeToLocalTime == id
unixTimeToLocalTime . localTimeToUnixTime /= id
@
-}
localTimeToUnixTime :: LocalTime -> UnixTime
localTimeToUnixTime t = UnixTime $
  dayToSecs (localDay t) + todHour tod*3600 + todMin tod*60 + truncate (todSec tod) -
  startOfTimeSecs
  where tod = localTimeOfDay t
{-# INLINE localTimeToUnixTime #-}

unixTimeToUTC :: UnixTime -> UTCTime
unixTimeToUTC t = UTCTime (toEnum $ startOfTimeDays + quot) (fromIntegral rem)
  where (quot,rem) = secondsFromEpoch t `quotRem` 86400

dayToSecs :: Day -> Int
dayToSecs =
  (86400*) .
  fromIntegral .
  toModifiedJulianDay

startOfTimeDays :: Int
startOfTimeDays = fromEnum (fromGregorian 1970 1 1)

startOfTimeSecs :: Int
startOfTimeSecs = dayToSecs (fromGregorian 1970 1 1)
