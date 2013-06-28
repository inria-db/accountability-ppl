module ObligationMatchingTest where

import Test.HUnit
import TestUtils

import ObligationMatching
import CommonTypes
import Syntax
import Events

testTime1 = (parsePPLTime "26 01 2012 11:54 AM")
testTime2 = (parsePPLTime "26 01 2012 07:54 AM")

sent1 = TriggerPersonalDataSent "http://www.inria.fr" 
actionDelete= ActionDeletePersonalData
obligation1 = Obligation sent1 actionDelete

time1 = TriggerAtTime testTime1
time2 = TriggerAtTime testTime2
obligation2 = Obligation time1 actionDelete
obligation3 = Obligation time2 actionDelete

test1 = TestCase (assertTrue  "An obligation is less permissive than itself - 1"	(lessPermissiveObligation obligation1 obligation1))

test2 = TestCase (assertTrue  "An obligation is less permissive than itself - 2"	(lessPermissiveObligation obligation2 obligation2))

test3 = TestCase (assertTrue  "Less permissive TriggerAtTime"	(lessPermissiveObligation obligation2 obligation3)) -- obligation2 has earlier time trigger


tests = TestList [
			  TestLabel "Obligation Matching 1" test1,
			  TestLabel "Obligation Matching 1" test2,
			  TestLabel "Obligation Matching 2" test3
		]

main = do runTestTT tests
