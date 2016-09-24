{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module DateTime
   (
     DT
   , DayOfWeek (..)
   , Match (And,Or,Not,Always,Never,DTMatch)
   , MatchRes
   , tick
   , untick
   , toDT
   , dowMatch
   , monthMatch
   , dayMatch
   , todMatch
   , match
   , split
   , model_match
   , model_split
   ) where

import           Control.Monad               (guard, when)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Array.Base             (unsafeAt)
import           Data.Array.IArray           (array)
import           Data.Array.Unboxed          (UArray)
import           Data.List                   (foldl')
import           Data.Time.Calendar          (fromGregorian, fromGregorianValid,
                                              toModifiedJulianDay)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Text.Printf

data DayOfWeek =
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Read,Enum)
$(deriveJSON defaultOptions ''DayOfWeek)

data DT = DT
  {
    year  :: {-# UNPACK #-} !Int
  , month :: {-# UNPACK #-} !Int
  , day   :: {-# UNPACK #-} !Int
  , dow   :: {-# UNPACK #-} !Int
  , tod   :: {-# UNPACK #-} !Int
  }

instance Eq DT where
  dt == dt' = tod dt == tod dt' &&
              day dt == day dt' &&
              month dt == month dt' &&
              year dt == year dt'

instance Ord DT where
  compare dt dt' =
    compare (year dt,month dt,day dt,tod dt)
            (year dt',month dt',day dt',tod dt')

instance Show DT where
  show dt = printf "%d-%02d-%02d %02d:%02d:%02d"
                   (year dt) (month dt) (day dt)
                   (tod dt `div` 3600) (((tod dt-s) `div` 60) `mod` 60) s
    where s = tod dt `mod` 60

toDT :: Int -> Int -> Int -> Int -> Int -> Int -> Either String DT
toDT year month day hour min sec = do
  dayObj <- maybe (Left "Invalid Year/Month/Day") return $ fromGregorianValid (fromIntegral year) month day
  when (0 > hour || hour > 23) $ Left "Invalid Hour"
  when (0 > min  || min  > 59) $ Left "Invalid Minute"
  when (0 > sec  || sec  > 59) $ Left "Invalid Second"
  let (_,_,dow) = toWeekDate dayObj
  return $ DT year month day (dow-1) (hour*3600 + min*60 + sec)

diffDT :: DT -> DT -> Int
diffDT dt1 dt2 = f dt2 - f dt1 + 1
  where f dt = let d = fromGregorian (fromIntegral (year dt))
                                     (month dt)
                                     (day dt)
               in secsInDay * fromInteger (toModifiedJulianDay d) + tod dt

------------------------------------------------------------
-- Constructing a DT match specification
------------------------------------------------------------

data Match = DowMatch (UArray Int Bool)
           | MonthMatch (UArray Int Bool)
           | DayMatch (UArray Int Bool)
           | TodMatch (Int,Int)
           | DTMatch (DT,DT)
           | Not Match
           | Or Match Match
           | And Match Match
           | Never
           | Always
           deriving (Show)

dowMatch :: [DayOfWeek] -> Match
dowMatch = DowMatch . array (0,6) . map (\d -> (fromEnum d,True))

monthMatch :: Integral a => [a] -> Match
monthMatch = MonthMatch . array (1,12) . map (\m -> (fromIntegral m,True)) . filter (<13) . filter (>0)

dayMatch :: Integral a => [a] -> Match
dayMatch = DayMatch . array (1,31) . map (\d -> (fromIntegral d,True)) . filter (<32) . filter (>0)

todMatch :: Integral a => [((a,a,a),(a,a,a))] -> Match
todMatch = foldr Or Never . map (\(x,y) -> TodMatch (toTod x,toTod y)) .
           filter (\(x,y) -> valid x && valid y && x <= y)
  where toTod (h,m,s) = fromIntegral $ h*3600 + m*60 + s
        valid (h,m,s) = 0 <= h && h <= 23 &&
                        0 <= m && m <= 59 &&
                        0 <= s && s <= 59

type MatchRes = [(Bool,Int,(DT,DT))]

------------------------------------------------------------
-- Running a match specification against a DT range
-- This is the brute-force version that will
-- construct the matched ranges by checking every
-- second in between.
--
-- This is for testing purposes.
------------------------------------------------------------

model_match :: Match -> DT -> Bool
model_match Always _ = True
model_match Never _ = False
model_match (DowMatch dows) dt = unsafeAt dows (dow dt)
model_match (MonthMatch months) dt = unsafeAt months (month dt)
model_match (DayMatch days) dt = unsafeAt days (day dt)
model_match (DTMatch (x,y)) dt = x <= dt && dt <= y
model_match (TodMatch (t,t')) dt = t <= tod dt && tod dt <= t'
model_match (Not m) dt = not (model_match m dt)
model_match (Or m m') dt = model_match m dt || model_match m' dt
model_match (And m m') dt = model_match m dt && model_match m' dt

model_split :: Match -> DT -> DT -> MatchRes
model_split m s e
  | s > e = []
  | otherwise = loop 0 s (ticks s e)
  where loop len prev [] = [(val,len,(s,prev))]
        loop !len prev (t:ts) =
          if model_match m t == val
          then loop (len+1) t ts
          else (val,len,(s,prev)) : model_split m t e
        val = model_match m s
        ticks s e
          | s == e = [e]
          | otherwise = s : ticks (tick s) e

------------------------------------------------------------
-- Running a match specification against a DT range
------------------------------------------------------------

split :: Match -> DT -> DT -> MatchRes
split m s e = map (\(b,(d1,d2)) -> (b,diffDT d1 d2,(d1,d2))) $ merge (loop s)
  where loop x | x > e = []
               | otherwise = let (b,dt) = match' m x
                                 end = min e dt
                             in (b,(x,end)) : loop (tick end)
        merge ((x1@(b1,(d1,_))) : (x2@(b2,(_,d2))) : xs)
          | b1 == b2 = merge ((b1,(d1,d2)) : xs)
          | otherwise = x1 : merge (x2:xs)
        merge xs = xs

match :: Match -> DT -> Bool
match m = fst . match' m

match' :: Match -> DT -> (Bool,DT)
match' Never dt = (False,DT 2100 1 1 4 0)
match' Always dt = (True,DT 2100 1 1 4 0)
match' (DowMatch dows) dt =
  (unsafeAt dows (dow dt),dt{tod=secsInDay-1})
match' (DayMatch days) dt =
  (unsafeAt days (day dt),dt{tod=secsInDay-1})
match' (MonthMatch months) dt =
  (unsafeAt months (month dt),dt{day=numDays (year dt) (month dt),tod=24*3600-1})
match' (TodMatch (t,t')) dt
  | tod dt < t = (False,dt{tod=t-1})
  | t <= tod dt && tod dt <= t' = (True,dt{tod=t'})
  | otherwise = (False,dt{tod=24*3600-1})
match' (DTMatch (s,e)) dt
  | dt < s = (False,untick s)
  | dt <= e = (True,e)
  | otherwise = (False,DT 2100 1 1 4 0)

match' (Not m) dt =
  let (b,dt') = match' m dt
  in (not b,dt')
match' (Or m1 m2) dt =
  let (b1,dt1) = match' m1 dt
      (b2,dt2) = match' m2 dt
  in (b1 || b2, min dt1 dt2)
match' (And m1 m2) dt =
  let (b1,dt1) = match' m1 dt
      (b2,dt2) = match' m2 dt
  in (b1 && b2, min dt1 dt2)

------------------------------------------------------------
-- Ticking DT by one second
------------------------------------------------------------

tick :: DT -> DT
tick = tickTOD
  where tickTOD (dt@DT{tod=s}) =
          let dt' = if s < secsInDay-1
                    then dt{ tod = s+1 }
                    else dt{ tod = 0 }
          in if s==(secsInDay-1) then tickDay dt' else dt'
        tickDay (dt@DT{day=d,dow=dayOfWeek,month=m,year=y}) =
          let dt' = dt{ dow = if dayOfWeek < 6 then dayOfWeek+1 else 0
                      , day = day'}
              day' = if d < 27
                     then d+1
                     else if d == numDays y m
                          then 1
                          else d+1
          in if day'==1 then tickMonth dt' else dt'
        tickMonth (dt@DT{month=m}) =
          let dt' = dt{ month = if m < 12 then m+1 else 1}
          in if m==12 then tickYear dt' else dt'
        tickYear (dt@DT{year=y}) = dt{year = y+1}

untick :: DT -> DT
untick = untickTOD
  where untickTOD (dt@DT{tod=s}) =
          let dt' = if s == 0
                    then dt{ tod = secsInDay-1 }
                    else dt{ tod = s-1 }
          in if s==0 then untickDay dt' else dt'
        untickDay (dt@DT{day=d,dow=dayOfWeek,month=m,year=y}) =
          let dt' = dt{ dow = if dayOfWeek == 0 then 6 else dayOfWeek-1
                      , day = day'}
              day' = if d == 1
                     then if m==1 then numDays (y-1) 12 else numDays y (m-1)
                     else d - 1
          in if d==1 then untickMonth dt' else dt'
        untickMonth (dt@DT{month=m}) =
          let dt' = dt{ month = if m == 1 then 12 else m-1}
          in if m==1 then untickYear dt' else dt'
        untickYear (dt@DT{year=y}) = dt{year = y-1}

numDays :: Int -> Int -> Int
numDays y m
  | m == 2 = if (mod y 4 == 0) && ((mod y 400 == 0) || not (mod y 100 == 0))
             then 29
             else 28
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
  | otherwise = 30

secsInDay :: Int
secsInDay = 86400
