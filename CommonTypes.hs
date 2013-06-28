module CommonTypes (
	  PiiId
	, PolicyId
	, Url
--	, TriggerId
	, TimeReference
	, module Time
	, exists
) where

import Time
import Data.Maybe 
import Data.List 

type PiiId = Int
type PolicyId = Int
type Url = String
--type TriggerId = Int
type TimeReference = DateTime

exists :: (a -> Bool) -> [a] -> Bool
exists f l = isJust (find f l)
