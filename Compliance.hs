module Compliance (
	  checkCompliance
	, checkLog
) where 

import CommonTypes
import Events
import Syntax
import ObligationCompliance
import AuthorizationsCheck
import NarrowLog
import GeneralCheck
import Data.List

checkCompliance :: Policy -> Log -> TimeReference -> Bool
checkCompliance (StickyPolicy authorizations obligations piiId) log timeReference = 
                                   checkLog l' 
                                && checkAuthorizations authorizations  l' 
                                && checkObligations obligations l' timeReference
                        where l' = narrowLog log piiId
checkCompliance _ _ _ = error "Compliance can only be checked with Sticky Policies"   


              


