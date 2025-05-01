module Main (main) where

import FromXML (parseXMLDocument)
import Lib (exitWithError)

main :: IO ()
main = do
  input <- readFile "example.xml"
  print input
  case parseXMLDocument input of
    Nothing -> exitWithError "Erreur de parsing"
    Just doc -> print doc