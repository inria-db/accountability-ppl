module ObligationMatchingTest where

import Test.HUnit
import TestUtils

import ObligationMatching
import CommonTypes
import Syntax

testTime1 = (parsePPLTime "26 01 2012 11:54 AM")
testTime2 = (parsePPLTime "26 01 2012 07:54 AM")

sent1 = TriggerPersonalDataSent "http://www.inria.fr" 
delete1 = ActionDeletePersonalData
obligation1 = Obligation sent1 delete1 

sent2 = TriggerPersonalDataSent "Url" 
obligation2 = Obligation sent2 delete1

time1 = TriggerAtTime testTime1
time2 = TriggerAtTime testTime2
obligation3 = Obligation time1 delete1
obligation4 = Obligation time2 delete1


obligationSet1 = [obligation1, obligation2] 

obligationSet2 = [obligation1]

obligationSet3 = [obligation1, obligation3]

obligationSet4 = [obligation1, obligation4]


test1 = TestCase (assertTrue  "Same ObligationSet"	(lessPermissiveObligations obligationSet1 obligationSet1))

test2 = TestCase (assertFalse "One obligation is missing"	(lessPermissiveObligations obligationSet2 obligationSet1))

test3 = TestCase (assertTrue  "More obligations than needed"	(lessPermissiveObligations obligationSet1 obligationSet2))

test4 = TestCase (assertTrue  "Different time"	(lessPermissiveObligations obligationSet3 obligationSet4))

tests = TestList [
			  TestLabel "Obligation Matching" test1
			, TestLabel "Obligation Matching" test2
			, TestLabel "Obligation Matching" test3
			, TestLabel "Obligation Matching" test4			
		]

main = do runTestTT tests
