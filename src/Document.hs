
data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe String
}

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

data Item = Item [Content]

data Document = Document {
    header :: Header,
    body :: [Content]
}