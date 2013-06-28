module AuthorizationsCheck (
	  checkAuthorizations
	, checkAllDS
) where

import CommonTypes
import Events
import Syntax
import ObligationCompliance
import Data.Maybe
import Data.List

-- Assumes log has been cleaned from other PII than the one beeing checked
-- Returns True iff all Use events in log have a corresponding authorization element in the policy and
-- there are no Sent events in case Downstream usage is forbidden. 
checkAuthorizations :: AuthorizationSet -> Log -> Bool
checkAuthorizations authzds log = checkAllDS authzds log

checkAllDS :: AuthorizationForDownStreamUsage -> Log -> Bool
checkAllDS authzds log = all (checkDSEvent authzds) log

checkDSEvent :: AuthorizationForDownStreamUsage -> Event -> Bool -- We don't care about events other than Sent
checkDSEvent (AuthorizationForDownStreamUsage False) (Sent _ _ _) = False
checkDSEvent _ _ = True
