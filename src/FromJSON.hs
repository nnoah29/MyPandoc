{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module FromJSON (
    parseJSONDocument
) where

import Document
import Lib (Parser, parseChar, parseString, parseSome, parseMany, parseAnyChar, (<|>), runParser)

parseJSONDocument :: String -> Maybe Document
parseJSONDocument input =
   case runParser parseDocumentJson input of
   Just (doc, "") -> Just doc
   _ -> Nothing

parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar " \n\t\r")

parseJsonString :: Parser String
parseJsonString = do
  _ <- parseChar '"'
  str <- parseMany (parseAnyChar (['a'..'z'] ++
    ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/()=<>+*&#%@[]{}\\"))
  _ <- parseChar '"'
  return str

parseKeyValue :: String -> Parser String
parseKeyValue key = do
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString key
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  value <- parseJsonString
  return value

parseHeaderJson :: Parser Header
parseHeaderJson = do
  _ <- parseWhitespace *> parseChar '{' *>  parseWhitespace
  title <- parseKeyValue "title"  
  author <- parseOptionalAuthor
  date <- parseOptionalDate
  _ <- parseWhitespace *> parseChar '}'
  return (Header title author date)

parseOptionalAuthor :: Parser (Maybe String)
parseOptionalAuthor = do
  _ <- parseWhitespace *>  parseChar ',' *>  parseWhitespace
         *> parseChar '"' *> parseString "author" *> parseChar '"'
            *> parseWhitespace *> parseChar ':' *> parseWhitespace
  author <- parseJsonString
  return (Just author)
  <|> return Nothing

parseOptionalDate :: Parser (Maybe String)
parseOptionalDate = do
  _ <- parseWhitespace *> parseChar ',' *>  parseWhitespace
         *> parseChar '"' *>  parseString "date" *> parseChar '"'
             *> parseWhitespace *> parseChar ':' *>  parseWhitespace
  date <- parseJsonString
  return (Just date)
  <|> return Nothing

parseParagraphJson :: Parser Content
parseParagraphJson = do
  _ <- parseWhitespace *> parseChar '{' *> parseWhitespace
         *> parseChar '"' *>  parseString "paragraph" *> parseChar '"'
            *> parseWhitespace *> parseChar ':' *> parseWhitespace
               *> parseChar '[' *>  parseWhitespace
  text <- parseJsonString
  _ <- parseWhitespace *> parseChar ']' *> parseWhitespace
        *> parseChar '}'
  return (Paragraph [Text text])

parseCodeBlockJson :: Parser Content
parseCodeBlockJson = do
  _ <- parseWhitespace *> parseChar '{' *>  parseWhitespace
        *> parseChar '"' *> parseString "codeblock" *> parseChar '"'
            *> parseWhitespace *> parseChar ':' *> parseWhitespace
  code <- parseJsonString
  _ <- parseWhitespace *> parseChar '}'
  return (CodeBlock code)

parseContentJson :: Parser Content
parseContentJson = parseParagraphJson <|> parseCodeBlockJson

parseBodyJson :: Parser [Content]
parseBodyJson = do
  _ <- parseWhitespace
  _ <- parseChar '['
  contents <- parseSomeContents
  _ <- parseWhitespace
  _ <- parseChar ']'
  return contents

parseSomeContents :: Parser [Content]
parseSomeContents = do
  content <- parseContentJson
  rest <- parseRestContents
  return (content : rest)
  <|> return []

parseRestContents :: Parser [Content]
parseRestContents = do
  _ <- parseWhitespace
  _ <- parseChar ','
  _ <- parseWhitespace
  content <- parseContentJson
  rest <- parseRestContents
  return (content : rest)
  <|> return []

parseDocumentJson :: Parser Document
parseDocumentJson = do
  _ <- parseWhitespace
  _ <- parseChar '{'
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "header"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  header <- parseHeaderJson
  _ <- parseWhitespace
  _ <- parseChar ','
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "body"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  body <- parseBodyJson
  _ <- parseWhitespace
  _ <- parseChar '}'
  _ <- parseWhitespace
  return (Document header body)
  