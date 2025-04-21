module FromXML (
    parseXMLDocument
) where

import Document

-- Prend une chaîne représentant un fichier XML, retourne éventuellement un Document
parseXMLDocument :: String -> Maybe Document

