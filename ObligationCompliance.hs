module ObligationCompliance (
	  checkObligations
	, checkObligation
) where 
import CommonTypes
import Events
import Syntax
import TriggerAtTimeCompliance
import TriggerPersonalDataDeletedCompliance
import TriggerPersonalDataSentCompliance

-- Assumes log has been cleaned from other PII than the one beeing checked
-- Returns True iff all Obligations have been complied with
checkObligations :: ObligationSet -> Log -> TimeReference ->  Bool 
checkObligations obligations log d = all (checkObligation log d) obligations

checkObligation :: Log -> TimeReference -> Obligation -> Bool
checkObligation log timeReference (Obligation trigger@(TriggerAtTime _) action)  = checkTriggerAtTime log trigger action timeReference
checkObligation log timeReference (Obligation trigger@(TriggerPersonalDataDeleted) action) = checkTriggerPersonalDataDeleted log trigger action timeReference
checkObligation log timeReference (Obligation trigger@(TriggerPersonalDataSent _) action) = checkTriggerPersonalDataSent log trigger action timeReference
