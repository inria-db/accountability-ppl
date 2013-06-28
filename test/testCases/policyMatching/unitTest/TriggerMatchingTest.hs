module TriggerMatchingTest where

import Test.HUnit
import TestUtils

import ObligationMatching
import CommonTypes
import Syntax

deleted1 = TriggerPersonalDataDeleted 
deleted2 = TriggerPersonalDataDeleted 

test1 = TestCase (assertTrue  "Delete" (lessPermissiveTrigger deleted1 deleted1)) -- no more maxdelay, so only case

sent1 = TriggerPersonalDataSent "id1" 
sent2 = TriggerPersonalDataSent "id2" 

test2 = TestCase (assertTrue  "Sent - same id"   (lessPermissiveTrigger sent1 sent1))
test3 = TestCase (assertFalse "Sent - different id" (lessPermissiveTrigger sent1 sent2))

testTimeReference = (parsePPLTime "26 01 2012 10:54 AM") 
testTimeReference2 = (parsePPLTime "26 01 2012 10:56 AM") 
testTimeReference3 = (parsePPLTime "25 01 2012 10:56 AM") 

time1 = TriggerAtTime testTimeReference
time2 = TriggerAtTime testTimeReference2 
time3 = TriggerAtTime testTimeReference3 

test4 = TestCase (assertTrue  "Time - equal start"   		(lessPermissiveTrigger time1 time1))
test5 = TestCase (assertTrue  "Time - later start"   					(lessPermissiveTrigger time2 time1))
test6 = TestCase (assertTrue "Time - outside window"   				(lessPermissiveTrigger time1 time3))
test7 = TestCase (assertFalse "Time - outside window"   				(lessPermissiveTrigger time3 time1))

tests = TestList [
			  TestLabel "Trigger Matching 1" test1
			, TestLabel "Trigger Matching 2" test2
			, TestLabel "Trigger Matching 3" test3
			, TestLabel "Trigger Matching 4" test4
			, TestLabel "Trigger Matching 5" test5
			, TestLabel "Trigger Matching 6" test6
			, TestLabel "Trigger Matching 7" test7
		]

main = do runTestTT tests
