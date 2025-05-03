module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr, hPutStrLn)
import Parser
import Document


import Lib


-- Parsing des arguments
getConfig :: [String] -> Config -> IO Config
getConfig [] config
    | null (ifile   config) = exitWithError "-i is required"
    | null (oformat config) = exitWithError "-f is required"
    | otherwise = return config
getConfig ("-i" : file   : xs) config = getConfig xs config { ifile   = file        }
getConfig ("-o" : file   : xs) config = getConfig xs config { ofile   = Just file   }
getConfig ("-f" : format : xs) config = getConfig xs config { oformat = format      }
getConfig ("-e" : format : xs) config = getConfig xs config { iformat = Just format }
getConfig (_ : _) _ = exitWithError "Invalid argument"

-- Fonction centrale
pandoc :: Config -> IO ()
pandoc conf = do
    input <- readFile (ifile conf)
    let format = getInputFormat conf
    document <- loadDocument format input
    output <- generateOutput (oformat conf) document

    case ofile conf of
        Just path -> writeFile path output
        Nothing   -> putStrLn output

main :: IO ()
main = do
    args <- getArgs
    conf <- getConfig args defaultConfig
    
