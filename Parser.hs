module Parser where
import Text.ParserCombinators.Parsec
import CommonTypes
import Events
import Syntax

testTimeReference = (parsePPLTime "03 06 2013 8:17 PM") -- Change this to edit the time of the check. dd mm yyyy       

spParse fn str targetPiiId = parse (parseSP targetPiiId) fn str

logParse fn str = parse parseFile fn str

parseFile = many parseEvent

parseEvent = try parseDeletedEvent 
              <|> try parseTimeEvent 
              <|> try parseDeleteEvent 
              <|> try parseSentEvent
               
number = many digit

parseAuthDS = do
                manyTill anyChar (try(lookAhead (string "AUTHZDOWNSTREAMUSAGE (ALLOWED, HJID) values"))) 
                string "AUTHZDOWNSTREAMUSAGE (ALLOWED, HJID) values (\'"
                authzDSbool <- many letter
                string "\', "
                return authzDSbool

parseSP:: PiiId -> (GenParser Char st Policy)
parseSP targetPiiId = do
                        manyTill anyChar (try(lookAhead parseDateTSQL))
                        startDate <- parseDateTSQL
                        authzDSbool <- manyTill parseAuthDS (string (show(targetPiiId))) 
                        -- keep parsing Downstream usage authorizations until the one for the specified PiiId is found
                        obligationSet <- many (try (parseObligation targetPiiId startDate))
                        return $ case (last authzDSbool) of -- using last because manyTill returns a list
                                      "true" -> StickyPolicy (AuthorizationForDownStreamUsage True) obligationSet targetPiiId
                                      "false" -> StickyPolicy (AuthorizationForDownStreamUsage False) obligationSet targetPiiId

parseObligation:: PiiId -> DateTime -> (GenParser Char st Obligation)
parseObligation targetPiiId startDate = do
                                          obligationAction <- try parseObligAction
                                          manyTill anyChar (try(lookAhead (string "insert into TRIGGER_")))
                                          string "insert into TRIGGER_"
                                          manyTill anyChar (try(lookAhead (string "insert into TRIGGER")))
                                          string "insert into TRIGGER"                    
                                          obligationTrigger <- (try (parseObligTriggerSent targetPiiId) 
                                                                 <|> try (parseObligTriggerTime targetPiiId startDate)) 
                                          return (Obligation obligationTrigger obligationAction)
                                                                                                                 
-- In this version we have only one possible action: ActionDeletePersonalData       
parseObligAction :: GenParser Char st Action
parseObligAction = do
                     manyTill anyChar (try(lookAhead (string "insert into ACTIONDELETEPERSONALDATA")))
                     try(string "insert into ACTIONDELETEPERSONALDATA")             
                     return ActionDeletePersonalData

-- only need to parse these two triggers for SP for now, because our only obligation action is delete                  
                
parseObligTriggerTime :: PiiId -> DateTime -> (GenParser Char st Trigger)
parseObligTriggerTime targetPiiId startDate = do
                                                string "ATTIME"
                                                manyTill anyChar (try(lookAhead (string "values (")))
                                                string "values ("
                                                digit
                                                string ", "
                                                string (show(targetPiiId)) -- second digit in value identifies PiiId to which obligation is related              
                                                return (TriggerAtTime startDate)
                                                
parseObligTriggerSent :: PiiId -> (GenParser Char st Trigger)
parseObligTriggerSent targetPiiId = do
                                      string "PERSONALDATASENT"   
                                      manyTill anyChar (try(lookAhead (string "insert into STICKYPOLICYTYPE (HJID) values (")))
                                      string "insert into STICKYPOLICYTYPE (HJID) values ("
                                      string (show(targetPiiId))               
                                      return (TriggerPersonalDataSent "toto@sap.com")                                                                        

parseDeletedEvent:: GenParser Char st Event 
parseDeletedEvent = do
                      spaces
                      many (try parseJunk)
                      paramDate <- parseDateTSQL
                      spaces
                      number
                      try(string " Query\tselect piiuniquei0_.id as id")
                      number
                      string "_, piiuniquei0_.uniqueId as uniqueId"
                      number
                      string "_ from piiuniqueid piiuniquei0_ cross"
                      manyTill anyChar (try(lookAhead (string "piiUniqueId_id="))) -- lookAhead makes manyTill not consume the string
                      string "piiUniqueId_id="
                      paramPiiId <- number
                      string " and "
                      manyTill anyChar (try(lookAhead (string "TriggerPersonalDataDeleted\'"))) 
                      string "TriggerPersonalDataDeleted\'"       
                      spaces                   
                      return (Deleted paramDate (read paramPiiId))           

parseTimeEvent:: GenParser Char st Event 
parseTimeEvent = do
                   spaces
                   many (try parseJunk)                   
                   paramDate <- parseDateT 
                   -- for log to be compliant, DateTime parameter must be same as in SP
                   space
                   string "TriggerAtTime triggered for"       
                   skipMany (noneOf ".")            
                   between (string ".jpg for pii name") (string ".jpg with id ") (skipMany (noneOf "."))
                   paramPiiId <- number
                   return (Time paramDate (read paramPiiId))

parseDeleteEvent:: GenParser Char st Event 
parseDeleteEvent = do
                     spaces
                     many (try parseJunk)     
                     paramDate <- parseDateT
                     skipMany (noneOf ".") 
                     between (string ".jpg for pii name") (string ".jpg with id ") (skipMany (noneOf "."))
                     paramPiiId <- number                    
                     string " has been deleted"                                       
                     return (Delete paramDate (read paramPiiId))
                                                                              
parseSentEvent:: GenParser Char st Event -- dsUsage is only sent for given PiiId!
parseSentEvent = do
                   spaces
                   many (try parseJunk)    
                   paramDate <- parseDateT
                   skipMany (noneOf ".")
                   between (string ".jpg for pii name") (string ".jpg with id ") (skipMany (noneOf "."))
                   paramPiiId <- number -- for log to be compliant, cannot Send PII that was deleted
                   string " was shared with " 
                   paramURL <- many (noneOf "\n\r\t ")   -- unix/windows newlines, tabs, space       
                   return (Sent paramDate (read paramPiiId) paramURL)

parseJunk =  try parseJunka 
              <|> try parseJunkb 
              <|> try parseJunkc 
              <|> try parseJunkd   
             
parseJunka = do
               spaces
               number
               space
               try(string "Query\tS") 
                <|> try(string "Query\ti") 
                <|> try(string "Query\tu") 
                <|> try(string "Query\tc") 
                <|> try(string "Query\ts") 
                <|> try(string "Query\td") 
                <|> try (string "C") 
                <|> try (string "I") 
                <|> try (string "Qui") 
                <|> try (string "Query\tD")
               skipMany (noneOf "\n") 
               spaces        
             
             
parseJunkb = do
               sqlDateLine
               space
               try(string "Query\tselect piit") 
                <|> try (string "Query\tselect piil") 
                <|> try (string "Query\tselect o") 
                <|> try(string "C") 
                <|> try(string "Query\tS") 
                <|> try (string "Query\tc")
               skipMany (noneOf "\n")
               spaces                          
             
parseJunkc = do
               sqlDateLine
               string " Query\tselect piiuniquei"
               number
               string "_.id as id"
               number
               string "_, piiuniquei"
               number
               string "_.uniqueId as uniqueId"
               number
               string "_ from piiuniqueid piiuniquei"
               number
               string "_\n"
               spaces       
               
parseJunkd = do 
               spaces            
               try(letter) <|> try(char '`')
               skipMany (noneOf "\n") 
               spaces        
                          
               
sqlDateLine = do
                count 6 digit
                space
                count 2 digit
                char ':'
                count 2 digit
                char ':'             
                count 2 digit
                spaces
                number        
        
parseDateT:: GenParser Char st DateTime 
parseDateT = do 
               result <- between (string "[") (string "]:") (many (noneOf "]"))
               return (parsePPLTimeNew result)
               
parseDateTSQL:: GenParser Char st DateTime 
parseDateTSQL = do 
                  sqlDate <- count 6 digit
                  sqlTime <- count 9 anyChar
                  return (parsePPLTimeSQL (sqlDate ++ sqlTime))     