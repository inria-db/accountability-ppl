module GeneralCheck (
	checkLog
) where

import CommonTypes
import Events
import Syntax
import Data.List
import Data.Maybe

-- General checks for log consistency. Assumes only events for one specific PII.
checkLog :: Log ->  Bool
checkLog [] = True
checkLog l = (noUseOfDataAfterDeletion l) -- no more condition on initial event

noUseOfDataAfterDeletion :: Log -> Bool
noUseOfDataAfterDeletion l = (isNothing deletionEventIndex) ||  
			     (noUseOfDataAfterDeletion' eventsAfterDeletion)
				where 
					deletionEventIndex = findIndex deletionEvent l
					eventsAfterDeletion = drop ((fromJust deletionEventIndex) + 1) l
								
deletionEvent :: Event -> Bool
deletionEvent (Delete _ _) = True
deletionEvent _ = False

noUseOfDataAfterDeletion' :: Log -> Bool
noUseOfDataAfterDeletion' l = all notUseOfDataEvent l 
