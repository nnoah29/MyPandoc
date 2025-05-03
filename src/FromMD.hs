module FromMD (
    parseMarkdownDocument
) where

import Document
-- Prend une chaîne représentant un fichier Markdown, retourne éventuellement un Document
parseMarkdownDocument :: String -> Maybe Document
parseMarkdownDocument _ = Just dummyDocument