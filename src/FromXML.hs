{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module FromXML (
    parseXMLDocument
) where

import Document
import Lib (Parser, parseChar, parseString, parseSome, parseMany, parseAnyChar, (<|>), runParser)

--prend une string et renvoie 1 document
parseXMLDocument :: String -> Maybe Document
parseXMLDocument input =
   case runParser parseDocumentXml input of
   Just (doc, "") -> Just doc
   _ -> Nothing

parseSpaces :: Parser String
parseSpaces = parseMany (parseAnyChar " \n\t\r")

parseAttribute :: String -> Parser String
parseAttribute attrName = do
  _ <- parseSpaces
  _ <- parseString attrName
  _ <- parseSpaces
  _ <- parseChar '='
  _ <- parseSpaces
  _ <- parseChar '"'
  val <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/"))
  _ <- parseChar '"'
  return val

parseOptionalAttribute :: String -> Parser (Maybe String)
parseOptionalAttribute attrName = (Just <$> parseAttribute attrName) <|> pure Nothing

parseOneAttribute :: Parser (String, String)
parseOneAttribute = do
  _ <- parseSpaces
  key <- parseSome (parseAnyChar ['a'..'z'])
  _ <- parseSpaces
  _ <- parseChar '='
  _ <- parseSpaces
  _ <- parseChar '"'
  val <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/"))
  _ <- parseChar '"'
  return (key, val)

parseHeaderAttributes :: Parser (String, Maybe String, Maybe String)
parseHeaderAttributes = do
    attrs <- parseMany parseOneAttribute
    return $ foldl accumulate ("", Nothing, Nothing) attrs
  where
    accumulate (t,a,d) (key,val) =
        case key of
            "title"  -> (val, a, d)
            "author" -> (t, Just val, d)
            "date"   -> (t, a, Just val)
            _        -> (t, a, d)


parseHeaderXml :: Parser Header
parseHeaderXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "header"
  _ <- parseSpaces
  (t, a, d) <- parseHeaderAttributes
  _ <- parseSpaces
  _ <- parseChar '>'
  _ <- parseSpaces
  _ <- parseString "</header>"
  return (Header t a d)

parseParagraphXml :: Parser Content
parseParagraphXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "paragraph"
  _ <- parseSpaces
  _ <- parseChar '>'
  txt <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/"))
  _ <- parseSpaces
  _ <- parseString "</paragraph>"
  return (Paragraph [Text txt])

parseCodeBlockXml :: Parser Content
parseCodeBlockXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "codeblock"
  _ <- parseSpaces
  _ <- parseChar '>'
  code <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/()\"'=<>"))
  _ <- parseSpaces
  _ <- parseString "</codeblock>"
  return (CodeBlock code)

parseSectionXml :: Parser Content
parseSectionXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "section"
  _ <- parseSpaces
  titleOpt <- parseOptionalAttribute "title"
  _ <- parseSpaces
  _ <- parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces
  _ <- parseString "</section>"
  return (Section titleOpt contents)

parseItemXml :: Parser Item
parseItemXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "item"
  _ <- parseSpaces
  _ <- parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces
  _ <- parseString "</item>"
  return (Item contents)

parseLinkXml :: Parser Content
parseLinkXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "link"
  _ <- parseSpaces
  textAttr <- parseAttribute "text"
  _ <- parseSpaces
  urlAttr <- parseAttribute "url"
  _ <- parseSpaces
  _ <- parseString "></link>"
  return (Link textAttr urlAttr)

parseImageXml :: Parser Content
parseImageXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "image"
  _ <- parseSpaces
  altAttr <- parseAttribute "alt"
  _ <- parseSpaces
  urlAttr <- parseAttribute "url"
  _ <- parseSpaces
  _ <- parseString "></image>"
  return (Image altAttr urlAttr)

parseListXml :: Parser Content
parseListXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "list"
  _ <- parseSpaces
  _ <- parseChar '>'
  items <- parseMany parseItemXml
  _ <- parseSpaces
  _ <- parseString "</list>"
  return (List items)

parseContentXml :: Parser Content
parseContentXml = parseParagraphXml
                <|> parseCodeBlockXml
                <|> parseSectionXml
                <|> parseListXml
                <|> parseLinkXml
                <|> parseImageXml

parseBodyXml :: Parser [Content]
parseBodyXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "body"
  _ <- parseSpaces
  _ <- parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces
  _ <- parseString "</body>"
  return contents

parseDocumentXml :: Parser Document
parseDocumentXml = do
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseSpaces
  _ <- parseString "document"
  _ <- parseSpaces
  _ <- parseChar '>'
  hdr <- parseHeaderXml
  contents <- parseBodyXml
  _ <- parseSpaces
  _ <- parseChar '<'
  _ <- parseChar '/'
  _ <- parseSpaces
  _ <- parseString "document"
  _ <- parseSpaces
  _ <- parseChar '>'
  return (Document hdr contents)
