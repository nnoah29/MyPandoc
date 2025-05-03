{-
-- EPITECH PROJECT, 2025
-- $PANDOC
-- File description:
-- $PANDOC
-}

module Document (
    Header(..),
    Content(..),
    Item(..),
    Document(..),
    dummyDocument
) where

data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving (Show)



data Content =
      Text String
    | Italic Content
    | Bold Content
    | Code String
    | Link String String
    | Image String String
    | Paragraph [Content]
    | Section (Maybe String) [Content]
    | CodeBlock String
    | List [Item]
    deriving (Show)

data Item = Item [Content] deriving (Show)

data Document = Document {
    header :: Header,
    body :: [Content]
} deriving (Show)

dummyDocument :: Document
dummyDocument = Document {
    header = Header {
        title = "",
        author = Nothing,
        date = Nothing
    },
    body = []
}