import Compliance
import Parser
import CommonTypes
import PrettyPrinter
import Control.Monad -- for when
import System.Environment -- for getArgs
import System.Directory -- for getDirectoryContents
import System.Exit -- for exitWith
import Data.List -- for sort
import Data.Either -- for rights
         
main = do
         listOfArgs <- getArgs;
         when (listOfArgs == []) $ do -- if no arguments
                                     putStrLn "Error: Argument must be an integer (PiiId)."
                                     exitWith (ExitFailure 1)    
         (targetPiiId:restargs) <- getArgs;
         when (isNotInteger targetPiiId) $ do -- if argument not integer
                                             putStrLn "Error: Argument must be an integer (PiiId)."
                                             exitWith (ExitFailure 1)
         logfiles <- readLogs -- get all .log files from current directory
         putStrLn "Found .log files in current directory:" 
         mapM_ print logfiles
         return logfiles >>= concatenate >>= mainParse -- concatenate all .log files, then parse them

readLogs = do
             putStrLn "Analysing .log files in current directory..."
             cd <- getCurrentDirectory
             files <- getDirectoryContents cd -- read all files in current directory
             logfiles <- return $ sort(filter (isSuffixOf ".log") files) -- keep only .log files and sort them
             when (logfiles == []) $ do -- if no .log files
                                          putStrLn "Error: current directory must contain at least one .log file."
                                          exitWith (ExitFailure 1)
             return logfiles 
             
concatenate [] = getContents -- get arguments
concatenate fs = concat `fmap` mapM readFile fs -- map readFile to list of arguments             

isNotInteger s = case reads s :: [(Integer, String)] of -- check whether a string is an Int
                      [(_, "")] -> False
                      _         -> True

mainParse mylog = do
                    (targetPiiId:restargs) <- getArgs;
                    putStr "\nPerforming compliance check for the PII with PiiId "
                    putStr targetPiiId
                    putStrLn " against its associated Sticky Policy on the above logs."
                    case (logParse "(unknown)" mylog) of
                         (Right s) ->  do
									     when (s ==[]) $ do 
									                       putStrLn "Error: no events found in .log files."
									                       exitWith (ExitFailure 1)
									     displayLog s
									     displaySP mylog targetPiiId
									     putStr "\nLog compliance check result: "
									     case (spParse "(unknown)" mylog (read(targetPiiId) :: PiiId)) of
									          (Right t) -> print (checkCompliance t s testTimeReference)
									          (Left u)  -> putStrLn "\nError: Sticky Policy not found for this PiiId."
                         (Left v) -> putStr $ show v	  
                      
displayLog logarg = do
                      putStrLn "\nParsed PPL event log:"
                      putStrLn "******************************************************************************************"
                      prettyPrint $ logarg
                      putStrLn "******************************************************************************************"
                 
displaySP logarg piiarg = do                
                            putStrLn "\nSticky Policy:"
                            putStrLn "******************************************************************************************"
                            when ((rights([spParse "(unknown)" logarg (read(piiarg) :: PiiId)])) == []) $ do
                                                                                                            putStr "Error: no Sticky Policy found for PiiId "
                                                                                                            putStr piiarg
                                                                                                            exitWith (ExitFailure 1)                                                
                            putStrLn (show(head(rights([spParse "(unknown)" logarg (read(piiarg) :: PiiId)]))))
                            putStrLn "******************************************************************************************"                                   
