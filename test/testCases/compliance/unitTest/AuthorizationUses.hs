module AuthorizationUses where

import Test.HUnit
import TestUtils
import TestValues

import AuthorizationsCheck
import CommonTypes
import Syntax
import Events

authDSFalse = (AuthorizationForDownStreamUsage False)

authDSTrue = (AuthorizationForDownStreamUsage True)

log1 = []

log2 = [(Sent (addDuration (Minutes 45) testStartTime) 1 "http://www.inria.fr")]

log3 = [(Time testStartTime 1)]


test1 = TestCase (assertTrue "Empty log - compliant" 
                  (checkAuthorizations authDSFalse log1))
test2 = TestCase (assertTrue  "Sent event OK - AuthorizationForDownStreamUsage True" 
                  (checkAuthorizations authDSTrue log2))     
test3 = TestCase (assertFalse  "Sent event despite no DS usage authorization - noncompliant" 
                  (checkAuthorizations authDSFalse log2))   
test4 = TestCase (assertTrue  "No Sent event, AuthorizationForDownStreamUsage True" 
                  (checkAuthorizations authDSTrue log3))
test5 = TestCase (assertTrue  "No Sent event, AuthorizationForDownStreamUsage False" 
                  (checkAuthorizations authDSFalse log3))

tests = TestList [
			  TestLabel "AuthorizationUses" test1
			, TestLabel "AuthorizationUses 10" test2
			, TestLabel "AuthorizationUses 11" test3
			, TestLabel "AuthorizationUses 15" test4
			, TestLabel "AuthorizationUses 16" test5
		]

main = do runTestTT tests
