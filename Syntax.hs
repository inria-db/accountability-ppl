module Syntax (
		Policy(..),
		AuthorizationForDownStreamUsage (..),
		Obligation(..),
		Trigger(..),
		Action(..),
		AuthorizationSet,
		ObligationSet
) where 
import CommonTypes

data Policy = Policy {	  
					  authorizations :: AuthorizationSet
					, obligations :: ObligationSet
					, policyId :: PolicyId
			  }
			| StickyPolicy {  	          
					  authorizations :: AuthorizationSet
					, obligations :: ObligationSet
					, piiId :: PiiId
			  } 
			deriving (Eq, Show, Read)


type AuthorizationSet = (AuthorizationForDownStreamUsage)

data AuthorizationForDownStreamUsage = AuthorizationForDownStreamUsage {dsUsage :: Bool} deriving (Eq, Show, Read)

type ObligationSet = [Obligation]


data Obligation = Obligation { trigger :: Trigger, action :: Action } deriving (Eq, Show, Read)

data Trigger = 	  TriggerAtTime {start :: DateTime}
				| TriggerPersonalDataDeleted
				| TriggerPersonalDataSent {url :: Url}
				deriving (Eq, Show, Read)

data Action = ActionDeletePersonalData 
			deriving (Eq, Show, Read)
