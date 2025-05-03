module Parser (
    getInputFormat,
    parseOrFail,
    loadDocument,
    generateOutput
) where

import Lib
import Data.List (isSuffixOf)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr, hPutStrLn)

import Document                     -- types Document, Header, etc.
import FromXML (parseXMLDocument)   -- à créer
import FromJSON (parseJSONDocument) -- à créer
import FromMD(parseMarkdownDocument) -- bonus
import ToXML (toXML)
import ToJSON (toJSON)
import ToMD (toMarkdown)


getInputFormat :: Config -> String
getInputFormat conf = case iformat conf of
    Just f  -> f
    Nothing -> detectFormat (ifile conf)
  where
    detectFormat file
      | ".xml" `isSuffixOf` file  = "xml"
      | ".json" `isSuffixOf` file = "json"
      | ".md" `isSuffixOf` file   = "markdown"
      | otherwise                 = "unknown"


parseOrFail :: (String -> Maybe Document) -> String -> IO Document
parseOrFail parser input = case parser input of
    Just doc -> return doc
    Nothing  -> exitWithError "Failed to parse document"


loadDocument :: String -> String -> IO Document
loadDocument fmt input = case fmt of
    "xml"      -> parseOrFail parseXMLDocument input
    "json"     -> parseOrFail parseJSONDocument input
    "markdown" -> parseOrFail parseMarkdownDocument input
    _          -> exitWithError "Unsupported input format"

generateOutput :: String -> Document -> IO String
generateOutput fmt document = case fmt of
    "xml"      -> return $ toXML document
    "json"     -> return $ toJSON document
    "markdown" -> return $ toMarkdown document
    _          -> exitWithError "Unsupported output format"
