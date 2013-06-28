module AuthorizationsMatching (
	  lessPermissiveAuthorizations
	, lessPermissiveAuthorizationDownstreamUsage
) where

import CommonTypes
import Syntax

lessPermissiveAuthorizations :: AuthorizationSet -> AuthorizationSet -> Bool
lessPermissiveAuthorizations lAuthzDownStream rAuthzDownStream =
		lessPermissiveAuthorizationDownstreamUsage lAuthzDownStream rAuthzDownStream

lessPermissiveAuthorizationDownstreamUsage :: AuthorizationForDownStreamUsage -> AuthorizationForDownStreamUsage -> Bool
lessPermissiveAuthorizationDownstreamUsage (AuthorizationForDownStreamUsage lAllowed) (AuthorizationForDownStreamUsage rAllowed) =
	rAllowed || (lAllowed == rAllowed)
