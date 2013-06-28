module LogGeneralCheck where

import Test.HUnit
import TestUtils
import TestValues

import GeneralCheck
import CommonTypes
import Syntax
import Events
        
        
log1 = [ (Delete dummyDate 2)]	

log2 = [ (Time testStartTime 1)
	   , (Delete (addDuration (Minutes 44) testStartTime) 1)] 


test1 = TestCase (assertTrue "Empty logs comply" (checkLog []))
test2 = TestCase (assertTrue "Single event" (checkLog log1))
test3 = TestCase (assertTrue "Multiple events" (checkLog log2))

tests = TestList [
				TestLabel "Log general check" test1
			  , TestLabel "Log general check" test2
			  , TestLabel "Log general check" test3
				 ]
		

main = do runTestTT tests


