module CheckObligationTriggerDataSentDelete where

import Test.HUnit
import TestUtils
import TestValues

import ObligationCompliance
import CommonTypes
import Syntax
import Events

testTime = (parsePPLTime "26 01 2012 10:54 AM")
testTime2 = (parsePPLTime "26 01 2012 11:54 AM")
testUrl = "Data Controller 2"
obligationDelete = Obligation 
				(TriggerPersonalDataSent testUrl) -- no more MaxDelay!
				(ActionDeletePersonalData)

log1 = [(Sent testTime testPiiId testUrl), 
        (Delete (addDuration minutes15 testTime) testPiiId)]
        
log2 = [(Sent testTime testPiiId testUrl)]

log3 = [(Sent testTime testPiiId testUrl), 
        (Delete (addDuration minutes15 testTime) testPiiId)]
	
log4 = [(Sent testTime testPiiId "Data Controller 3")]

test1 = TestCase (assertTrue   "Deletion happens" 
		  (checkObligation log1 testTimeReference obligationDelete))
		  
test2 = TestCase (assertFalse  "Obligation not complied with" 
		  (checkObligation log2 testTimeReference obligationDelete))
		  
		  
test4 = TestCase (assertTrue   "Trigger is not fired because data was not sent to this DC" 
		  (checkObligation log4 testTimeReference obligationDelete))
		  
-- time reference tests

test5 = TestCase (assertFalse "Obligation not complied with" 
		  (checkObligation log2 testTimeReference2 obligationDelete))
		  
test6 = TestCase (assertTrue   "Deletion, time reference in the past" 
		  (checkObligation log1 testTimeReference3 obligationDelete))

tests = TestList [
			    TestLabel "TriggerDataSent - Delete 1" test1
			  , TestLabel "TriggerDataSent - Delete 2" test2
			  , TestLabel "TriggerDataSent - Delete 6" test4
			  , TestLabel "TriggerDataSent - Delete 7" test5
			  , TestLabel "TriggerDataSent - Delete 8" test6
		]

main = do runTestTT tests

