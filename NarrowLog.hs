module NarrowLog (
	narrowLog
) where

import CommonTypes
import Events

-- Keeps only Events in the log corresponding to the PII SP being analyzed.
narrowLog :: Log -> PiiId -> Log
narrowLog log id = filter (\e -> (Events.piiId e) == id) log
