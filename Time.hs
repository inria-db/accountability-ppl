module Time (
	  betweenDate
	, parsePPLTime
	, parsePPLTimeNew
	, parsePPLTimeSQL	
	, maxTime
	, minTime
	, module Data.DateTime
	, Duration(..)
	, addDuration
	, mult
	, periodCount
) where

import System.Locale
import System.IO
import Data.Time
import Data.Time.Format
import Data.DateTime -- http://hackage.haskell.org/packages/archive/datetime/0.1/doc/html/Data-DateTime.html
import Data.Maybe

-- boolean function that returns whether a date is between two other dates
betweenDate :: DateTime -> DateTime -> DateTime -> Bool
betweenDate dateToCheck start end = start <= end && start <= dateToCheck && dateToCheck <= end

-- parses time data from the format used in the PPL logs:

timeFormat = "%d %m %Y %l:%M %p"
-- 26 01 2012 11:48 AM

parsePPLTime :: String -> DateTime
parsePPLTime s = fromJust (parseDateTime timeFormat s)

-- parser for new format:

timeFormatNew = "%d/%m/%Y %l:%M:%S"
-- 12/11/2012 15:48:49

parsePPLTimeNew :: String -> DateTime
parsePPLTimeNew s = fromJust (parseDateTime timeFormatNew s)

-- parser for SQL format:

timeFormatSQL = "%d%m%y %l:%M:%S"
-- 121112 15:48:49

parsePPLTimeSQL :: String -> DateTime
parsePPLTimeSQL s = fromJust (parseDateTime timeFormatSQL s)

maxTime :: DateTime -> DateTime -> DateTime
maxTime d1 d2 = if d1 < d2 then d2 else d1 

minTime :: DateTime -> DateTime -> DateTime
minTime d1 d2 = if d1 < d2 then d1 else d2

-- Duration

data Duration = Minutes Int | Seconds Int deriving (Eq, Show, Ord)

addDuration :: Duration -> DateTime -> DateTime
addDuration (Minutes m) date = addMinutes' m date
addDuration (Seconds s) date = addSeconds (fromIntegral s) date -- converts s from integer to Seconds

mult :: Duration -> Int -> Duration -- multiply a duration
mult (Minutes m) i = Minutes (fromIntegral(m *i))
mult (Seconds m) i = Seconds (fromIntegral(m *i))

-- counts the number of periods between two dates, with period length as a parameter
periodCount :: DateTime -> DateTime -> Duration -> Int 
periodCount start end (Minutes m) = fromIntegral((diffMinutes end start)) `div` m
periodCount start end (Seconds s) = fromIntegral((diffSeconds end start)) `div` s
