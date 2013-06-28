module ObligationComplianceUtils (
		correspondingTriggerEvent
	  , checkTriggerCompliance
	  , correspondingTriggerEvents
	  , correspondingEventsExist
	  , correspondingNotificationEventCondition
	  , correspondingDeleteEventCondition
) where 
import CommonTypes
import Events
import Syntax
import Data.Maybe 
import Data.List 

-- TriggerAtTime, TriggerPersonalDeleted
correspondingTriggerEvent :: Log -> Trigger -> (Trigger -> Event -> Bool) -> Maybe Event
correspondingTriggerEvent log trigger condition = find (condition trigger) log

checkTriggerCompliance :: Log -> Action -> (Event -> Action -> Event -> Bool) -> TimeReference -> Event -> Bool
checkTriggerCompliance log a condition timeReference event = obligationActionExists
	where 
		obligationActionExists = (exists (condition event a) log)

-- TriggerPersonalDataSet, TriggerPersonalDataUsedForPurpose, TriggerPersonalDataAccessed

correspondingTriggerEvents :: Log -> Trigger -> (Trigger -> Event -> Bool) -> [Event]
correspondingTriggerEvents log trigger condition = filter (condition trigger) log

correspondingEventsExist :: [Event] -> Action -> Log -> (Event -> Action -> Event -> Bool) -> TimeReference -> Bool 
correspondingEventsExist [] _ _ _ _  = True
correspondingEventsExist events a log f timeReference = all (checkTriggerCompliance log a f timeReference) events 

correspondingNotificationEventCondition :: Event  -> Action -> Event -> Bool
correspondingNotificationEventCondition _ _ _ = False

correspondingDeleteEventCondition :: Event ->  Action -> Event -> Bool
correspondingDeleteEventCondition e (ActionDeletePersonalData) (Delete time _) = True
correspondingDeleteEventCondition _ _ _ = False