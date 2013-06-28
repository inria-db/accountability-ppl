module ObligationMatching (
	  lessPermissiveObligations
	, lessPermissiveObligation
	, lessPermissiveTrigger
) where

import CommonTypes
import Syntax

lessPermissiveObligations :: ObligationSet -> ObligationSet -> Bool
lessPermissiveObligations lObligationSet rObligationSet = 
	all (existsLessPermissiveObligation lObligationSet) rObligationSet

existsLessPermissiveObligation :: ObligationSet -> Obligation -> Bool
existsLessPermissiveObligation obligationSet r = exists (\l -> lessPermissiveObligation l r) obligationSet

lessPermissiveObligation :: Obligation -> Obligation -> Bool
lessPermissiveObligation lObligation rObligation = 
		lessPermissiveTrigger (Syntax.trigger lObligation) (Syntax.trigger rObligation) 

lessPermissiveTrigger :: Trigger -> Trigger -> Bool
lessPermissiveTrigger (TriggerAtTime lStart) (TriggerAtTime rStart) = 
--	lStart >= rStart && (addDuration lStart) <= (addDuration rStart) Unsure here... may need to fix this later
	lStart >= rStart
lessPermissiveTrigger (TriggerPersonalDataDeleted) (TriggerPersonalDataDeleted) = True
--	lMaxDelay <= rMaxDelay
lessPermissiveTrigger (TriggerPersonalDataSent lDownStreamId) (TriggerPersonalDataSent rDownStreamId) = 
--	lDownStreamId == rDownStreamId && lMaxDelay <= rMaxDelay
	lDownStreamId == rDownStreamId
lessPermissiveTrigger _ _ = False
