module AuthorizationUsageMatchingTest where

import Test.HUnit
import TestUtils

import AuthorizationsMatching
import CommonTypes
import Syntax


trueDownstreamUsage = AuthorizationForDownStreamUsage True
falseDownstreamUsage = AuthorizationForDownStreamUsage False 

test1 = TestCase (assertTrue  "Downstream usage requested and allowed" 
					(lessPermissiveAuthorizationDownstreamUsage trueDownstreamUsage trueDownstreamUsage))
test2 = TestCase (assertTrue  "Downstream usage not requested and allowed" 
					(lessPermissiveAuthorizationDownstreamUsage falseDownstreamUsage trueDownstreamUsage))
test3 = TestCase (assertFalse  "Downstream usage requested and not allowed" 
					(lessPermissiveAuthorizationDownstreamUsage trueDownstreamUsage falseDownstreamUsage))
test4 = TestCase (assertTrue  "Downstream usage neither requested nor allowed" 
					(lessPermissiveAuthorizationDownstreamUsage falseDownstreamUsage falseDownstreamUsage))

tests = TestList [
			  TestLabel "AuthorizationDownstreamUsage" test1
			, TestLabel "AuthorizationDownstreamUsage" test2
			, TestLabel "AuthorizationDownstreamUsage" test3
			, TestLabel "AuthorizationDownstreamUsage" test4
		]

main = do runTestTT tests
