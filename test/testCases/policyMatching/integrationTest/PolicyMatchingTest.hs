module PolicyMatchingTest where

import Test.HUnit
import TestUtils

import PolicyMatching
import CommonTypes
import Syntax

sent1 = TriggerPersonalDataSent "http://www.inria.fr" 
delete1 = ActionDeletePersonalData
obligation1 = Obligation sent1 delete1 

sent2 = TriggerPersonalDataSent "Url" 
obligation2 = Obligation sent2 delete1

rObligationSet = [obligation1, obligation2]

rAuthorizations = (AuthorizationForDownStreamUsage True)

lPolicy = Policy rAuthorizations rObligationSet 1 -- third argument is PolicyId
rPolicy = Policy rAuthorizations rObligationSet 2

test1 = TestCase (assertTrue  "Obligations comply in less time"	(lessPermissivePolicy lPolicy rPolicy))

lObligationSet2 = [obligation1] -- policy

lPolicy2 = Policy rAuthorizations lObligationSet2 3
rPolicy2 = Policy rAuthorizations rObligationSet 4

test2 = TestCase (assertFalse  "One obligation is not fulfilled"	(lessPermissivePolicy lPolicy2 rPolicy2))

lPolicy3 = Policy rAuthorizations rObligationSet 5
rPolicy3 = Policy rAuthorizations lObligationSet2 6

test3 = TestCase (assertTrue  "Less obligations in policy"	(lessPermissivePolicy lPolicy3 rPolicy3))

tests = TestList [
			  TestLabel "Obligation Matching" test1
			, TestLabel "Obligation Matching" test2
			, TestLabel "Obligation Matching" test3
		]

main = do runTestTT tests
