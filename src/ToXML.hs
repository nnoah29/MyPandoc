{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module ToXML (
    toXML
) where

import Document

gen_attr :: String -> Maybe String -> String
gen_attr name (Just val) = " " ++ name ++ "=\"" ++ val ++ "\""
gen_attr _ Nothing = ""

toXMLHeader :: Header -> String
toXMLHeader (Header tit auth date) =
    "<header title=\"" ++ tit ++ "\"" ++
    gen_attr "author" auth ++
    gen_attr "date" date ++
    "></header>"

toXMLItem :: Item -> String
toXMLItem (Item contents) =
    "<item>" ++ concatMap toXMLContent contents ++ "</item>"

toXMLContent :: Content -> String
toXMLContent (Paragraph texts) =
    "<paragraph>" ++ concatMap toXMLText texts ++ "</paragraph>"

toXMLContent (CodeBlock code) =
    "<codeblock>" ++ code ++ "</codeblock>"

toXMLContent (Section titleOpt contents) =
    "<section" ++ gen_attr "title" titleOpt ++ ">" ++
    concatMap toXMLContent contents ++
    "</section>"

toXMLContent (List items) =
    "<list>" ++ concatMap toXMLItem items ++ "</list>"

toXMLContent (Link text url) =
     "<link " ++ gen_attr "text" (Just text) ++
     " " ++ gen_attr "url" (Just url) ++ "></link>"

toXMLContent (Image alt url) =
    "<image alt=\"" ++ alt ++ "\" url=\"" ++ url ++ "\"></image>"

toXMLText :: Content -> String
toXMLText (Text txt) = txt

toXMLBody :: [Content] -> String
toXMLBody contents =
    "<body>" ++ concatMap toXMLContent contents ++ "</body>"

--prend un document et renvoie une string
toXML :: Document -> String
toXML (Document hdr contents) =
    "<document>" ++
    toXMLHeader hdr ++
    toXMLBody contents ++
    "</document>"
