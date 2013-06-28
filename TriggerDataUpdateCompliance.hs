module TriggerDataUpdateCompliance (
	  checkTriggerDataUpdate
) where 
import CommonTypes
import Events
import Syntax
import ObligationComplianceUtils
import Data.Maybe

checkTriggerDataUpdate :: Log -> Trigger -> Action -> TimeReference -> Bool
		
checkTriggerDataUpdate log t@(TriggerDataUpdate)  a@(ActionDeletePersonalData) timeReference = 
	error "It makes no sense to have an action delete on trigger update"


correspondingUpdateEventCondition :: Trigger -> Event -> Bool
correspondingUpdateEventCondition TriggerDataUpdate (DataUpdate _ _ _) = True
correspondingUpdateEventCondition _ _ = False
