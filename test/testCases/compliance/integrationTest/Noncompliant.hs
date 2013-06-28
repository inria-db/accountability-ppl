module Noncompliant where

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
	(	 AuthorizationForDownStreamUsage True)
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

dcLog1 = [ (Time testStartTime 1)
		 , (Sent (addDuration (Minutes 45) testStartTime) 1 "http://www.inria.fr")]

dcLog2 = [ (Time testStartTime 1)] 

dcLog3 = [ (Time testStartTime 1)
		 , (Sent (addDuration (Minutes 45) testStartTime) 1 "http://www.inria.fr")
		 , (Delete (addDuration (Minutes 50) testStartTime) 1)]

dcLog4 = [ (Time testStartTime 1)
		 , (Delete (addDuration (Minutes 44) testStartTime) 1)
		 , (Delete (addDuration (Minutes 45) testStartTime) 1)
		 ] -- double delete

dcLog5 = [ (Time testStartTime 1)
		 , (Delete (addDuration (Minutes 30) testStartTime) 1)
		 , (Sent (addDuration (Minutes 35) testStartTime) 1 "http://www.inria.fr")
		 ]		 

------------------------------------------------------------------------------
-- check compliance of logs with respect to an externally given Sticky Policy
------------------------------------------------------------------------------

test1 = TestCase (assertFalse  "Non-compliant 1 - send obligation not fulfilled" 
		 (checkCompliance sp1 dcLog1 testTimeReference)) 

test2 = TestCase (assertFalse  "Non-compliant 2 - deleted obligation unfulfilled" 
		  (checkCompliance sp2 dcLog2 testTimeReference)) 
		  
test3 = TestCase (assertFalse  "Non-compliant 3 - Downstream usage forbidden" 
		  (checkCompliance sp2 dcLog3 testTimeReference)) 	
		  
test4 = TestCase (assertFalse  "Non-compliant 4 - Double deletion of same PII" 
		  (checkCompliance sp2 dcLog4 testTimeReference))
		  
test5 = TestCase (assertFalse  "Non-compliant 5 - Sent after Delete for same PII" 
		  (checkCompliance sp3 dcLog5 testTimeReference))		   			  	  

tests = TestList [
			  TestLabel "Non-Compliant 1 - send obligation not fulfilled" test1
			, TestLabel "Non-compliant 2 - deleted obligation unfulfilled" test2
			, TestLabel "Non-compliant 3 - Downstream usage forbidden" test3
			, TestLabel "Non-compliant 4 - Double deletion of same PII" test4		
			, TestLabel "Non-compliant 5 - Sent after Delete for same PII" test5								
		]

main = do runTestTT tests
