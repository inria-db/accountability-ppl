module TriggerAtTimeCompliance (
	  checkTriggerAtTime
) where 
import CommonTypes
import Events
import Syntax
import ObligationComplianceUtils
import Data.Maybe
import Data.List

-- BEGIN TRIGGER AT TIME 

checkTriggerAtTime :: Log -> Trigger -> Action -> TimeReference -> Bool

checkTriggerAtTime log t@(TriggerAtTime start)  a@(ActionDeletePersonalData) timeReference = 
    timeReference <= start || correspondantEventCheck
	where
		triggerEventFound = correspondingTriggerEvent log t triggerAtTimeEventCondition
		correspondantEventCheck = checkTriggerCompliance log a correspondingDeleteEventCondition timeReference triggerEvent		
		triggerEvent = (fromJust triggerEventFound)

triggerAtTimeEventCondition :: Trigger -> Event -> Bool
triggerAtTimeEventCondition (TriggerAtTime start) (Time time _) = start == time 
triggerAtTimeEventCondition _ _ = False
