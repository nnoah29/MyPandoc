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
import Lib (Parser(..), parseChar, parseString, parseSome, parseMany, parseAnyChar, (<|>), runParser)

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
  _ <- parseSpaces *>  parseString attrName *> parseSpaces
        *> parseChar '=' *> parseSpaces *> parseChar '"'
  let allowed = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/"
  val <- parseSome (parseAnyChar allowed)
  _ <- parseChar '"'
  return val

parseOptionalAttribute :: String -> Parser (Maybe String)
parseOptionalAttribute attrName = (Just <$>
                                    parseAttribute attrName) <|>
                                        pure Nothing

parseOneAttribute :: Parser (String, String)
parseOneAttribute = do
  _ <- parseSpaces
  key <- parseSome (parseAnyChar ['a'..'z'])
  _ <- parseSpaces *> parseChar '=' *>  parseSpaces
        *> parseChar '"'
  val <- parseSome (parseAnyChar (['a'..'z'] ++
    ['A'..'Z'] ++ ['0'..'9'] ++ " .,:;!?-_/"))
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
  _ <- parseSpaces *>  parseChar '<' *>  parseSpaces
        *> parseString "header" *> parseSpaces
  (t, a, d) <- parseHeaderAttributes
  _ <- parseSpaces *>  parseChar '>' *> parseSpaces
        *> parseString "</header>"
  return (Header t a d)

parseParagraphXml :: Parser Content
parseParagraphXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces
        *> parseString "paragraph" *> parseSpaces *> parseChar '>'
  txt <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++
            ['0'..'9'] ++ " .,:;!?-_/"))
  _ <- parseSpaces *>  parseString "</paragraph>"
  return (Paragraph [Text txt])

parseCodeBlockXml :: Parser Content
parseCodeBlockXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces *> parseString "codeblock"
  _ <- parseSpaces *> parseChar '>'
  code <- parseMany (parseCharNot '<')
  _ <- parseSpaces *> parseString "</codeblock>"
  return (CodeBlock code)

parseCharNot :: Char -> Parser Char
parseCharNot c = Parser $ \input ->
  case input of
    []     -> Nothing
    (x:xs) -> if x /= c then Just (x, xs) else Nothing


parseSectionXml :: Parser Content
parseSectionXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces
         *> parseString "section" *> parseSpaces
  titleOpt <- parseOptionalAttribute "title"
  _ <- parseSpaces *> parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces *> parseString "</section>"
  return (Section titleOpt contents)

parseItemXml :: Parser Item
parseItemXml = do
  _ <- parseSpaces *> parseChar '<' *>  parseSpaces
        *> parseString "item" *> parseSpaces *> parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces *> parseString "</item>"
  return (Item contents)

parseLinkXml :: Parser Content
parseLinkXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces
        *> parseString "link" *> parseSpaces
  textAttr <- parseAttribute "text"
  _ <- parseSpaces
  urlAttr <- parseAttribute "url"
  _ <- parseSpaces *> parseString "></link>"
  return (Link textAttr urlAttr)

parseImageXml :: Parser Content
parseImageXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces
        *> parseString "image" *> parseSpaces
  altAttr <- parseAttribute "alt"
  _ <- parseSpaces
  urlAttr <- parseAttribute "url"
  _ <- parseSpaces *>  parseString "></image>"
  return (Image altAttr urlAttr)

parseListXml :: Parser Content
parseListXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces *> parseString "list"
        *> parseSpaces *> parseChar '>'
  items <- parseMany parseItemXml
  _ <- parseSpaces *> parseString "</list>"
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
  _ <- parseSpaces *> parseChar '<' *> parseSpaces *> parseString "body"
        *> parseSpaces *> parseChar '>'
  contents <- parseMany parseContentXml
  _ <- parseSpaces *> parseString "</body>"
  return contents

parseDocumentXml :: Parser Document
parseDocumentXml = do
  _ <- parseSpaces *> parseChar '<' *> parseSpaces *> parseString "document"
         *> parseSpaces *> parseChar '>'
  hdr <- parseHeaderXml
  contents <- parseBodyXml
  _ <- parseSpaces *> parseChar '<' *> parseChar '/'
        *> parseSpaces *> parseString "document"
            *> parseSpaces *> parseChar '>'
  return (Document hdr contents)
