module CheckObligationTriggerAtTimeDelete where

import Test.HUnit
import TestUtils
import TestValues

import ObligationCompliance
import CommonTypes
import Syntax
import Events

obligationDelete = Obligation 
				(TriggerAtTime testStartTime) -- no more MaxDelay!
				(ActionDeletePersonalData)

log1 = [ Time testStartTime testPiiId, 
         Delete (addDuration minutes15 testStartTime) testPiiId] 
         
log2 = [ Time testStartTime testPiiId] -- time event, no delete event

log3 = [ Time testStartTime testPiiId, 
         Delete (addDuration minutes15 testStartTime) testPiiId]
         
log4 = [ Time testStartTime testPiiId, 
         Delete (addDuration minutes15 testStartTime) testPiiId, 
         Delete (addDuration minutes16 testStartTime) testPiiId] 
         
log5 = []

log7 = [ Delete (addDuration minutes15 testStartTime) testPiiId,
	     Time testStartTime testPiiId] -- reverse line order but right chronology

test1 = TestCase (assertTrue  "Deletion performed" 
                  (checkObligation log1 testTimeReference obligationDelete))
                  
test2 = TestCase (assertFalse "Obligation not complied with" -- no posterior action at all
                  (checkObligation log2 testTimeReference obligationDelete))

                  
test4 = TestCase (assertTrue  "Deletion exists, other deletion for different triggerId exists " 
                  (checkObligation log4 testTimeReference obligationDelete))
                  
test5 = TestCase (assertFalse "Empty log" 
                  (checkObligation log5 testTimeReference obligationDelete)) -- noncompliant

                  
test7 = TestCase (assertTrue "Reverse line order" 
                  (checkObligation log7 testTimeReference obligationDelete))

-- time reference tests
test8 = TestCase (assertFalse "8 - Obligation not complied with" 
                  (checkObligation log2 testTimeReference2 obligationDelete))
-- The former assertTrue for this, along with "Obligation not complied but time remains" can no longer be used as a justification 
-- for compliance because there is no maximum delay anymore, so if one accepts that the obligation could still be fulfilled
-- in the future, one can never conclude noncompliance since there is no deadline! so I will change this to noncompliant.             
                  
                  
test9 = TestCase (assertTrue  "Obligation complied with, time reference in the past" 
                  (checkObligation log1 testTimeReference3 obligationDelete))

tests = TestList [
	        TestLabel "TriggerAtTime - Delete" test1
		  , TestLabel "TriggerAtTime - Delete" test2
		  , TestLabel "TriggerAtTime - Delete" test4
		  , TestLabel "TriggerAtTime - Delete 5" test5
		  , TestLabel "TriggerAtTime - Delete 7" test7
		  , TestLabel "TriggerAtTime - Delete" test8
		  , TestLabel "TriggerAtTime - Delete" test9
		]

main = do runTestTT tests
