module Lib (
    exitWithError
    ) where

--sortie d'erreur
exitWithError :: String -> IO a
exitWithError msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)