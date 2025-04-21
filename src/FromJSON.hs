module FromJSON (
    parseJSONDocument
) where

import Document

-- Prend une chaîne représentant un fichier JSON, retourne éventuellement un Document
parseJSONDocument :: String -> Maybe Document
