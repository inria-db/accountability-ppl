module TriggerPersonalDataSentCompliance (
	  checkTriggerPersonalDataSent
) where 
import CommonTypes
import Events
import Syntax
import ObligationComplianceUtils

checkTriggerPersonalDataSent :: Log -> Trigger -> Action -> TimeReference -> Bool
		
checkTriggerPersonalDataSent log t@(TriggerPersonalDataSent _)  a@(ActionDeletePersonalData) timeReference = 
	correspondingEventsExist triggerEvents a log correspondingDeleteEventCondition timeReference
	where
		triggerEvents = correspondingTriggerEvents log t correspondingSentEventCondition 

correspondingSentEventCondition :: Trigger -> Event -> Bool
correspondingSentEventCondition (TriggerPersonalDataSent triggerUrl) (Sent _ _ url) = url == triggerUrl
correspondingSentEventCondition _ _ = False
