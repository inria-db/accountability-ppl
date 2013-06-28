module NarrowLogCheck where

import Test.HUnit
import TestUtils
import TestValues

import NarrowLog
import CommonTypes
import Syntax
import Events

testTime = testStartTime

log1 = [(Deleted testTime 2), -- piiID 2
        (Deleted testTime 3), -- piiID 3
        (Deleted testTime 2)] -- piiID 2

log2 = [(Deleted testTime 3), -- piiID 3
        (Deleted testTime 2), -- piiID 2
        (Deleted testTime 4)] -- piiID 4
        
log3 = []

test1 = TestCase (assertNotEmpty "Should not be empty" (narrowLog log1 2)) -- only keep events with piiID 2

test2 = TestCase (assertCount    "Should have 2 elements" (narrowLog log1 2) 2)

test3 = TestCase (assertEqual    "Exact elements" 
                  [(Deleted testTime 2), 
                   (Deleted testTime 2)] (narrowLog log1 2))
                   
test4 = TestCase (assertEmpty    "Should be empty" (narrowLog log1 1)) -- only keep events with piiID 1

test5 = TestCase (assertEmpty    "Should be empty" (narrowLog log3 2)) -- empty log

test6 = TestCase (assertEqual    "Exact elements" 
                  [(Deleted testTime 2)] (narrowLog log2 2))

tests = TestList [
		    TestLabel "Narrow log" test1
	 	  , TestLabel "Narrow log for PiiID 2 - element count" test2
		  , TestLabel "Narrow log for PiiID 2" test3
		  , TestLabel "Narrow log for PII not appearing in initial log" test4
		  , TestLabel "Narrow log of an empty log is empty" test5
		  , TestLabel "Narrow log for PiiID 2 for log2" test6
		]

main = do runTestTT tests


