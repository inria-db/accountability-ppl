module PolicyMatching (
	  matchPolicies
	, lessPermissivePolicy
) where

import CommonTypes
import Syntax
import ObligationMatching
import AuthorizationsMatching
import Data.Maybe

-- takes two policies as input and returns the second one if the first one is stricter than the second one
matchPolicies :: Policy -> Policy -> Maybe Policy 
matchPolicies lPolicy@(Policy _ _ _) rPolicy@(Policy _ _ _) = 
	if lessPermissivePolicy lPolicy rPolicy then 
		(Just rPolicy) 
	else Nothing

-- takes two policies as input and returns whether the first one is stricter than the other
lessPermissivePolicy :: Policy -> Policy -> Bool 
lessPermissivePolicy lPolicy rPolicy = 
		lessPermissiveAuthorizations (Syntax.authorizations lPolicy) (Syntax.authorizations rPolicy)
	&&  lessPermissiveObligations (Syntax.obligations lPolicy) (Syntax.obligations rPolicy)
