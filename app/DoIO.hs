module DoIO where

import           ProjConfig
import           System.IO  (hFlush, stdout)

errNoSuchCmd:: IO ()
errNoSuchCmd = putStrLn "Cannot recognise commands."

putHelp :: IO ()
putHelp = putStrLn $ "Usage: " ++ cmdName ++ " [commands]"

askWhatToAdd :: IO String
askWhatToAdd = prompt "Add what? "

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

