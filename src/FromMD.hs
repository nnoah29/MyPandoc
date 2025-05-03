{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module FromMD (
    parseMarkdownDocument
) where

import Document
-- Prend une chaîne représentant un fichier Markdown, retourne éventuellement un Document
import Data.Maybe
import Data.List
import Data.Char

--PARSER PRINCIPAL
parseMarkdownDocument :: String -> Maybe Document
parseMarkdownDocument input =
    let ls = lines input
    in case parseHeader ls of
        Nothing -> Nothing
        Just (hdr, rest) -> Just $ Document hdr (parseBody rest)

--PARSE HEADER
parseHeader :: [String] -> Maybe (Header, [String])
parseHeader (l1:ls)
    | "# " `isPrefixOf` l1 =
        let titleText = drop 2 l1
            (authorLine, rest1) = extractMeta "Author: " ls
            (dateLine, rest2)   = extractMeta "Date: " rest1
        in Just (Header titleText authorLine dateLine, dropWhile null rest2)
parseHeader _ = Nothing

extractMeta :: String -> [String] -> (Maybe String, [String])
extractMeta prefix (l:ls)
    | prefix `isPrefixOf` l = (Just (drop (length prefix) l), ls)
    | otherwise = (Nothing, l:ls)
extractMeta _ [] = (Nothing, [])

--PARSE BODY
parseBody :: [String] -> [Content]
parseBody [] = []
parseBody ls =
    let (content, rest) = parseBlock ls
    in content ++ parseBody rest

parseBlock :: [String] -> ([Content], [String])
parseBlock [] = ([], [])
parseBlock (l:ls)
    | "## " `isPrefixOf` l = parseSection (l:ls)
    | "```" `isPrefixOf` l = parseCodeBlock (l:ls)
    | "- "  `isPrefixOf` l = parseListBlock (l:ls)
    | all isSpace l        = parseBlock ls  -- skip empty lines
    | otherwise            = parseParagraphBlock (l:ls)

parseSection :: [String] -> ([Content], [String])
parseSection (l:ls) =
    let title = Just (drop 3 l)
        (sectionLines, rest) = span (\x -> not ("## " `isPrefixOf` x)) ls
        content = parseBody sectionLines
    in ([Section title content], rest)
parseSection [] = ([], [])

parseCodeBlock :: [String] -> ([Content], [String])
parseCodeBlock (_:ls) =
    let (codeLines, rest) = break (isPrefixOf "```") ls
    in ([CodeBlock (unlines codeLines)], drop 1 rest)
parseCodeBlock [] = ([], [])

parseListBlock :: [String] -> ([Content], [String])
parseListBlock ls =
    let (items, rest) = span (isPrefixOf "- ") ls
        parsed = map (Item . (:[]) . Paragraph . parseInline . drop 2) items
    in ([List parsed], rest)

parseParagraphBlock :: [String] -> ([Content], [String])
parseParagraphBlock ls =
    let (paraLines, rest) = span (\x -> not (null x) && not (isBlockStart x)) ls
        content = Paragraph (concatMap parseInline paraLines)
    in ([content], rest)

isBlockStart :: String -> Bool
isBlockStart l = any (`isPrefixOf` l) ["## ", "```", "- "]

--PARSE INLINE
parseInline :: String -> [Content]
parseInline "" = []
parseInline str
    | Just (inside, rest) <- extractBetween "**" str =
        Bold (Paragraph (parseInline inside)) : parseInline rest
    | Just (inside, rest) <- extractBetween "*" str =
        Italic (Paragraph (parseInline inside)) : parseInline rest
    | Just (inside, rest) <- extractBetween "`" str =
        Code inside : parseInline rest
    | otherwise =
        let (txt, rest) = break (\c -> c == '`' || c == '*') str
        in Text txt : parseInline rest

-- Utilitaire pour extraire contenu entre délimiteurs
extractBetween :: String -> String -> Maybe (String, String)
extractBetween delim s =
    case stripPrefix delim s of
        Just rest ->
            let (inside, rest') = breakOn delim rest
            in case stripPrefix delim rest' of
                Just after -> Just (inside, after)
                Nothing    -> Nothing
        Nothing -> Nothing

breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn delim s = go "" s
  where
    go acc [] = (reverse acc, [])
    go acc rem@(x:xs)
        | delim `isPrefixOf` rem = (reverse acc, rem)
        | otherwise = go (x:acc) xs
