module TriggerPersonalDataDeletedCompliance (
	  checkTriggerPersonalDataDeleted
) where 
import CommonTypes
import Events
import Syntax
import ObligationComplianceUtils
import Data.Maybe

checkTriggerPersonalDataDeleted :: Log -> Trigger -> Action -> TimeReference -> Bool
		
checkTriggerPersonalDataDeleted log t@(TriggerPersonalDataDeleted)  a@(ActionDeletePersonalData) timeReference = 
	error "It makes no sense to have an Action Delete on Trigger Deleted"


correspondingDeletedEventCondition :: Trigger -> Event -> Bool
correspondingDeletedEventCondition TriggerPersonalDataDeleted (Deleted _ _) = True
correspondingDeletedEventCondition _ _ = False
