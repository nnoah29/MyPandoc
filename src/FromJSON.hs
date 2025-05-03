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
  str <- parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/()=<>+*&#%@[]{}\\"))
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
  _ <- parseWhitespace
  _ <- parseChar '{'
  _ <- parseWhitespace
  
  title <- parseKeyValue "title"  
  author <- parseOptionalAuthor
  date <- parseOptionalDate
  _ <- parseWhitespace
  _ <- parseChar '}'
  
  return (Header title author date)

parseOptionalAuthor :: Parser (Maybe String)
parseOptionalAuthor = do
  _ <- parseWhitespace
  _ <- parseChar ','
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "author"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  author <- parseJsonString
  return (Just author)
  <|> return Nothing

parseOptionalDate :: Parser (Maybe String)
parseOptionalDate = do
  _ <- parseWhitespace
  _ <- parseChar ','
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "date"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  date <- parseJsonString
  return (Just date)
  <|> return Nothing

parseParagraphJson :: Parser Content
parseParagraphJson = do
  _ <- parseWhitespace
  _ <- parseChar '{'
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "paragraph"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  _ <- parseChar '['
  _ <- parseWhitespace
  text <- parseJsonString
  _ <- parseWhitespace
  _ <- parseChar ']'
  _ <- parseWhitespace
  _ <- parseChar '}'
  return (Paragraph [Text text])

parseCodeBlockJson :: Parser Content
parseCodeBlockJson = do
  _ <- parseWhitespace
  _ <- parseChar '{'
  _ <- parseWhitespace
  _ <- parseChar '"'
  _ <- parseString "codeblock"
  _ <- parseChar '"'
  _ <- parseWhitespace
  _ <- parseChar ':'
  _ <- parseWhitespace
  code <- parseJsonString
  _ <- parseWhitespace
  _ <- parseChar '}'
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
  