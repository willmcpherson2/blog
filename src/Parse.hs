module Parse (Document(..), Element(..), Part(..), Style(..), parse) where
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Maybe (fromJust)

newtype Document = Document [Element]
  deriving Show

data Element = Element Style [Part]
  deriving Show

data Part
  = CharPart Char
  | ElementPart Element
  deriving Show

data Style
  = Rule
  | Title
  | Heading
  | Subheading
  | CodeBlock
  | Bold
  | Italics
  | CodeInline
  | Link
  | Paragraph
  deriving Show

type ParseMaybe = String -> Maybe (String, Element)

type Parse = String -> (String, Element)

parse :: String -> Document
parse source =
  let (source', ele) = parseBlock source
  in
    case source' of
      [] -> Document [ele]
      _ -> let Document eles = parse source' in Document $ ele : eles

parseBlock :: Parse
parseBlock source =
  let source' = dropWhile isSpace source
  in fromJust $ asum $ map ($ source') blockParsers

parseEle :: ParseMaybe
parseEle source = asum $ map ($ source) elementParsers

blockParsers :: [ParseMaybe]
blockParsers =
  [ mkParser Rule "---" "\n"
  , mkParser Title "# " "\n"
  , mkParser Heading "## " "\n"
  , mkParser Subheading "### " "\n"
  , mkParser CodeBlock "``\n" "\n``\n"
  , mkParser Paragraph "" "\n\n"
  ]

elementParsers :: [ParseMaybe]
elementParsers =
  [ mkParser Bold "**" "**"
  , mkParser Italics "*" "*"
  , mkParser CodeInline "`" "`"
  , mkParser Link "[" "]"
  ]

mkParser :: Style -> String -> String -> ParseMaybe
mkParser style start end source =
  let (prefix, suffix) = splitAt (length start) source
  in
    if prefix == start
      then let (source', ele) = parseRest suffix in Just (source', ele)
      else Nothing
  where
    parseRest :: Parse
    parseRest source =
      let (prefix, suffix) = splitAt (length end) source
      in
        if prefix == end
          then (suffix, Element style [])
          else case style of
            CodeBlock -> parseChar source
            _ -> case parseEle source of
              Just (source, ele) ->
                let (source', Element style parts) = parseRest source
                in (source', Element style (ElementPart ele : parts))
              Nothing -> parseChar source
    parseChar :: Parse
    parseChar source = case source of
      char : source ->
        let (source', Element style parts) = parseRest source
        in (source', Element style (CharPart char : parts))
      [] -> (source, Element style [])
