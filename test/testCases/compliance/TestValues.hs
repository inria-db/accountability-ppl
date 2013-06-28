module TestValues (
	  maxDelay15
	, maxDelay30
	, minutes15
	, minutes16
	, minutes30
	, minutes31
	, testPiiId
	, dummyDate
	, testPurpose
	, testMaxDelay
	, testStartTime
	, testTimeReference
	, testTimeReference2
	, testTimeReference3
) where

import Test.HUnit
import CommonTypes

testMaxDelay = maxDelay30

maxDelay15 = minutes15
maxDelay30 = minutes30

minutes15 = Minutes 15
minutes16 = Minutes 16
minutes30 = Minutes 30
minutes31 = Minutes 31

testPiiId :: Int
testPiiId = fromIntegral 1

dummyDate = startOfTime

testPurpose = "Marketing"

testStartTime = (parsePPLTime "26 01 2012 10:54 AM")

testTimeReference = (parsePPLTime "27 01 2012 10:54 AM") -- 24 hours after testStartTime
testTimeReference2 = (parsePPLTime "26 01 2012 10:56 AM") -- 2 minutes later than testStartTime
testTimeReference3 = (parsePPLTime "25 01 2012 10:56 AM") -- BEFORE testStartTime!
