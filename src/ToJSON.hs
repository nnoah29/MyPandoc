{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module ToJSON (
    toJSON
) where

import Document

maybeField :: String -> Maybe String -> String
maybeField _ Nothing = ""
maybeField name (Just val) = ", \"" ++ name ++ "\": \"" ++ val ++ "\""

toJSONHeader :: Header -> String
toJSONHeader (Header tit auth date) =
    "{\"title\": \"" ++ tit ++ "\"" ++
    maybeField "author" auth ++
    maybeField "date" date ++
    "}"

toJSONText :: Content -> String
toJSONText (Text txt) = "\"" ++ txt ++ "\""

toJSONItem :: Item -> String
toJSONItem (Item contents) =
    "{\"item\": [" ++ joinWithCommas (map toJSONContent contents) ++ "]}"

toJSONContent :: Content -> String
toJSONContent (Paragraph texts) =
    "{\"paragraph\": [" ++ joinWithCommas (map toJSONText texts) ++ "]}"

toJSONContent (CodeBlock code) =
    "{\"codeblock\": \"" ++ code ++ "\"}"

toJSONContent (Section titleOpt contents) =
    "{\"section\": {" ++
    (case titleOpt of
        Just title -> "\"title\": \"" ++ title ++ "\", "
        Nothing -> "") ++
    "\"contents\": [" ++ joinWithCommas (map toJSONContent contents) ++ "]}}"

toJSONContent (List items) =
    "{\"list\": [" ++ joinWithCommas (map toJSONItem items) ++ "]}"

toJSONContent (Link text url) =
    "{\"link\": {\"text\": \"" ++ text ++ "\", \"url\": \"" ++ url ++ "\"}}"

toJSONContent (Image alt url) =
    "{\"image\": {\"alt\": \"" ++ alt ++ "\", \"url\": \"" ++ url ++ "\"}}"

joinWithCommas :: [String] -> String
joinWithCommas = foldr (\x acc -> if null acc then x else x ++ ", " ++ acc) ""

toJSONBody :: [Content] -> String
toJSONBody contents =
    "[" ++ joinWithCommas (map toJSONContent contents) ++ "]"

toJSON :: Document -> String
toJSON (Document hdr contents) =
    "{\"header\": " ++
    toJSONHeader hdr ++
    ", \"body\": " ++
    toJSONBody contents ++
    "}"