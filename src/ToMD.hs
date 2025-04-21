module ToMarkdown (
    toMarkdown
) where

import Document

-- Prend un Document et retourne une chaîne Markdown
toMarkdown :: Document -> String