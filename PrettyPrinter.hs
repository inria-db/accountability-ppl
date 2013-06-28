module PrettyPrinter where
import Events

prettyPrint :: [Event] -> IO ()
prettyPrint [] = putStr ""
prettyPrint (x:xs) = do
                       putStrLn $ prettyPrintEvent x
                       prettyPrint xs
                       
prettyPrintEvent :: Event -> String
prettyPrintEvent (Time param1 param2) =  "[" ++ show(param1) ++ "]: " ++ "TriggerAtTime" ++ " PiiId:" ++ show([param2])
prettyPrintEvent (Deleted param1 param2) = "[" ++ show(param1) ++ "]: " ++ "TriggerDeleted" ++ " PiiId:" ++ show([param2])
prettyPrintEvent (Sent param1 param2 param4) = "[" ++ show(param1) ++ "]: " ++ "TriggerSent" ++ " PiiId:" ++ show([param2]) ++ " URL:" ++ show([param4])
prettyPrintEvent (Delete param1 param2) = "[" ++ show(param1) ++ "]: " ++ "ActionDelete" ++ " PiiId:" ++ show([param2])
