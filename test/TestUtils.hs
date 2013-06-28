module TestUtils (
	  assertTrue
	, assertFalse
	, assertEmpty
	, assertCount
	, assertNotEmpty
) where

import Test.HUnit

-- Bool
assertTrue :: String -> Bool -> Assertion
assertTrue msg b = assertEqual msg True b

assertFalse :: String -> Bool -> Assertion
assertFalse msg b = assertTrue msg (not b)

-- Lists
assertEmpty :: String -> [a] -> Assertion 
assertEmpty msg l = assertCount msg l 0

assertNotEmpty :: Eq a => Show a => String -> [a] -> Assertion 
assertNotEmpty msg l = assertFalse msg ((length l) == 0)

assertCount :: String -> [a] -> Int -> Assertion
assertCount msg l n = assertTrue msg ((length l) == n)

