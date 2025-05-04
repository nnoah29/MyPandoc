{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module ToMD (
    toMarkdown
) where

import Document

-- Convertit l'en-tête
toMarkdownHeader :: Header -> String
toMarkdownHeader (Header t a d) =
    "# " ++ t ++ "\n"
    ++ maybe "" (\x -> "Author: " ++ x ++ "\n") a
    ++ maybe "" (\x -> "Date: " ++ x ++ "\n") d

-- Convertit chaque élément de contenu
toMarkdownContent :: Content -> String
toMarkdownContent content = case content of
    Text s          -> renderText s
    Italic c        -> renderItalic c
    Bold c          -> renderBold c
    Code s          -> renderCode s
    Paragraph cs    -> renderParagraph cs
    Section t cs    -> renderSection t cs
    CodeBlock s     -> renderCodeBlock s
    List items      -> renderList items
    Link txt url    -> renderLink txt url
    Image alt src   -> renderImage alt src

-- === FONCTIONS DE RENDU POUR CHAQUE TYPE ===

renderText :: String -> String
renderText = id

renderItalic :: Content -> String
renderItalic c = "*" ++ flattenContent c ++ "*"

renderBold :: Content -> String
renderBold c = "**" ++ flattenContent c ++ "**"

renderCode :: String -> String
renderCode s = "`" ++ s ++ "`"

renderParagraph :: [Content] -> String
renderParagraph cs = concatMap toMarkdownContent cs ++ "\n"

renderSection :: Maybe String -> [Content] -> String
renderSection (Just title) cs =
    "## " ++ title ++ "\n\n" ++ unlines (map toMarkdownContent cs)
renderSection Nothing cs =
    "## \n\n" ++ unlines (map toMarkdownContent cs)

renderCodeBlock :: String -> String
renderCodeBlock s = "```\n" ++ s ++ "```\n"

renderList :: [Item] -> String
renderList = unlines . map renderItem

renderItem :: Item -> String
renderItem (Item cs) = "- " ++ concatMap toMarkdownContent cs

renderLink :: String -> String -> String
renderLink txt url = "[" ++ txt ++ "](" ++ url ++ ")"

renderImage :: String -> String -> String
renderImage alt src = "![" ++ alt ++ "](" ++ src ++ ")"

-- Récupère le texte contenu dans un paragraphe ou autre
flattenContent :: Content -> String
flattenContent (Paragraph cs) = concatMap toMarkdownContent cs
flattenContent c              = toMarkdownContent c

toMarkdown :: Document -> String
toMarkdown (Document hdr body) =
    toMarkdownHeader hdr ++ "\n\n" ++ unlines (map toMarkdownContent body)

-- Prend un Document et retourne une chaîne Markdown
