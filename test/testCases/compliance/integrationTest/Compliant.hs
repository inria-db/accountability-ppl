module Compliant where

import Test.HUnit
import TestUtils
import TestValues

import Compliance
import CommonTypes
import Syntax
import Events

past15 = addDuration minutes15 testStartTime
past30 = addDuration minutes30 testStartTime
past45 = addDuration minutes45 testStartTime
minutes45 = (Minutes 45)
minutes60 = (Minutes 60)

---------------------------------------------------------------------
-- Sample Sticky Policies
---------------------------------------------------------------------

sp1 = StickyPolicy
	(AuthorizationForDownStreamUsage True)
	[
		 Obligation (TriggerPersonalDataSent "http://www.inria.fr") 
                  (ActionDeletePersonalData)
	] 1 -- this parameter is the piiId

sp2 = StickyPolicy
	(AuthorizationForDownStreamUsage False)
	[
		 Obligation (TriggerAtTime testStartTime) 
                  (ActionDeletePersonalData)

	] 1

sp3 = StickyPolicy
	(AuthorizationForDownStreamUsage True) 
	[] 1

---------------------------------------------------------------------
-- Sample logs
---------------------------------------------------------------------

dcLog1 = [ (Time testStartTime 1)]

dcLog2 = [ (Time testStartTime 1)
		 , (Sent (addDuration (Minutes 45) testStartTime) 1 "http://www.google.fr")]

dcLog3 = [ (Time testStartTime 1)
		 , (Sent (addDuration (Minutes 45) testStartTime) 1 "http://www.inria.fr")
		 , (Delete (addDuration (Minutes 50) testStartTime) 1)]

dcLog4 = [ (Time testStartTime 1)
		 , (Delete (addDuration (Minutes 44) testStartTime) 1)] 

dcLog5 = [ (Time testStartTime 1)
		 , (Delete (addDuration (Minutes 44) testStartTime) 1)
		 , (Deleted (addDuration (Minutes 45) testStartTime) 1)]

dcLog6 = [ (Time testStartTime 1)
		 , (Sent (addDuration (Minutes 40) testStartTime) 1 "http://www.inria.fr")
		 , (Delete (addDuration (Minutes 44) testStartTime) 1)
		 , (Deleted (addDuration (Minutes 45) testStartTime) 1)]

------------------------------------------------------------------------------
-- check compliance of logs with respect to an externally given Sticky Policy
------------------------------------------------------------------------------
test1 = TestCase (assertTrue "Compliant - no obligation to fulfill" (checkCompliance sp1 dcLog1 testTimeReference)) 

test2 = TestCase (assertTrue "Compliant - sent to other url than specified one" 
		 (checkCompliance sp1 dcLog2 testTimeReference)) 

test3 = TestCase (assertTrue "Compliant - send obligation triggered and fulfilled" 
		 (checkCompliance sp1 dcLog3 testTimeReference)) 

test4 = TestCase (assertTrue "Compliant - Delete obligation fulfilled" 
		  (checkCompliance sp2 dcLog4 testTimeReference)) 

test5 = TestCase (assertTrue "Compliant - Delete obligation fulfilled + irrelevant event" 
		  (checkCompliance sp2 dcLog5 testTimeReference)) 

test6 = TestCase (assertTrue "Compliant - no obligation to fulfill" (checkCompliance sp3 dcLog6 testTimeReference)) 

tests = TestList [
			  TestLabel "Compliant - no obligation to fulfill" test1
			, TestLabel "Compliant - sent to other url than specified one" test2
			, TestLabel "Compliant - send obligation triggered and fulfilled" test3
			, TestLabel "Compliant - Delete obligation fulfilled" test4
			, TestLabel "Compliant - Delete obligation fulfilled + irrelevant event" test5
			, TestLabel "Compliant - no obligation to fulfill" test6
		 ]

main = do runTestTT tests


