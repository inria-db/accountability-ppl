module Events (
	  Event(..)
	, Log
	, notUseOfDataEvent
) where 
import CommonTypes
import Syntax

type Log = [Event]

data Event =   	  
		  Time 
				{ time :: DateTime, piiId :: PiiId}
		| Deleted 
				{ time :: DateTime, piiId :: PiiId}
		| Sent 
				{ time :: DateTime, piiId :: PiiId, url :: Url}
		| Delete 
				{ time :: DateTime, piiId :: PiiId} 
		deriving (Eq, Show, Read) -- this makes parsing string to log possible!!!
--		deriving (Eq, Show)		

notUseOfDataEvent :: Event -> Bool -- what does this do? Should Sent be here too?
notUseOfDataEvent (Time _ _) = True
notUseOfDataEvent (Deleted _ _) = True
notUseOfDataEvent _ = False


