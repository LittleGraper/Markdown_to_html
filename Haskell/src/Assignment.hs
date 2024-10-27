{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Data.Char (isSpace)
import Instances (Parser (..))
import Parser (inlineSpace, is, noneof, oneof, parseEmptyLines, parsePositiveInt, space, spaces, spaces1, string)
import Data.Maybe (fromMaybe)


-- | ----------------------------------------------------------------- |
-- | ------------------- Algebraic Data Types(ADTs) ------------------ |
-- | ----------------------------------------------------------------- |
-- Define Algebraic Data Types(ADTs)
data ADT = ADT [Block]
  deriving (Show, Eq)

-- Define inline Text Modifiers
data Inline
  = PlainText String
  | Italic [Inline]
  | Bold [Inline]
  | Strikethrough [Inline]
  | Link String [Inline]
  | InlineCode String
  | FootnoteInline Int
  deriving (Show, Eq)

-- Define block-level elements
data Block
  = Heading Int [Inline]
  | FreeText [Inline]
  | FootnoteRef Int String
  | Image String String String -- alt, url, title
  | BlockQuote [Block]
  | CodeBlock String String -- language, content
  | OrderedList [ListItem]
  | UnorderedList [ListItem]
  | Table TableRow [TableRow] -- table header, other rows
  deriving (Show, Eq)

-- Define list items for ordered lists
data ListItem = ListItem
  { isSublist :: Bool, -- is sublist or not
    liContent :: [Inline] -- list item content
  }
  deriving (Show, Eq)

-- Define table row
data TableRow = TableRow [Inline]
  deriving (Show, Eq)

-- | ----------------------------------------------------------------- |
-- | ---------------------- Text Modifiers --------------------------- |
-- | ----------------------------------------------------------------- |
-- >>> parse parseItalic "_italic_"
-- Result >< Italic [PlainText "italic"]
parseItalic :: Parser Inline
parseItalic = do
  _ <- is '_'
  content <- some (noneof "_\n")
  _ <- is '_'
  return $ Italic [PlainText content]

-- >>> parse parseBold "**bold**"
-- Result >< Bold [PlainText "bold"]
parseBold :: Parser Inline
parseBold = do
  _ <- string "**"
  content <- some (noneof "*\n")
  _ <- string "**"
  return $ Bold [PlainText content]

-- >>> parse parseStrikethrough "~~  strikethrough~~"
-- Result >< Strikethrough [PlainText "  strikethrough"]

parseStrikethrough :: Parser Inline
parseStrikethrough = do
  _ <- string "~~"
  content <- some (noneof "~\n")
  _ <- string "~~"
  return $ Strikethrough [PlainText content]

parseLink :: Parser Inline
parseLink = do
  _ <- is '['
  linkText <- some (noneof "]")
  _ <- is ']'
  _ <- is '('
  url <- some (noneof ")")
  _ <- is ')'
  return $ Link url [PlainText linkText]

parseInlineCode :: Parser Inline
parseInlineCode = do
  _ <- is '`'
  content <- some (noneof "`\n")
  _ <- is '`'
  return $ InlineCode content

parseFootnoteInline :: Parser Inline
parseFootnoteInline = do
  _ <- string "[^"
  number <- parsePositiveInt
  _ <- is ']'
  return $ FootnoteInline number

parsePlainText :: Parser Inline
parsePlainText = do
  text <- some (noneof "_*~`[^\n")
  return $ PlainText text

parseInline :: Parser Inline
parseInline =
  parseItalic
    <|> parseBold
    <|> parseStrikethrough
    <|> parseLink
    <|> parseInlineCode
    <|> parseFootnoteInline
    <|> parsePlainText

-- Convert inline elements to HTML
convertInline :: Inline -> String
convertInline (PlainText text) = text
convertInline (Italic inlines) = "<em>" ++ concatMap convertInline inlines ++ "</em>"
convertInline (Bold inlines) = "<strong>" ++ concatMap convertInline inlines ++ "</strong>"
convertInline (Strikethrough inlines) = "<del>" ++ concatMap convertInline inlines ++ "</del>"
convertInline (Link url inlines) = "<a href=\"" ++ url ++ "\">" ++ concatMap convertInline inlines ++ "</a>"
convertInline (InlineCode code) = "<code>" ++ code ++ "</code>"
convertInline (FootnoteInline n) = "<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>"

-- | --------------------------------------------------------
-- | --------------- Block Level elements -------------------
-- | --------------------------------------------------------

-- FreeText
parseFreeText :: Parser Block
parseFreeText = do
  -- Parse leading and trailing empty lines
  _ <- optional parseEmptyLines
  -- Parse leading whitespaces
  _ <- spaces
  content <- some parseInline
  _ <- spaces
  _ <- optional parseEmptyLines
  return $ FreeText content

-- Heading
hashHeading :: Parser Block
hashHeading = do
  _ <- inlineSpace -- skip leading spaces, tabs, and newlines
  hashes <- some (is '#') -- one or more '#' characters
  _ <- is ' ' -- A space after the '#' characters
  content <- many parseInline -- content of the heading
  _ <- inlineSpace
  let level = length hashes
  guard (level >= 1 && level <= 6) -- level must be between 1 and 6
  return $ Heading level content

-- Alternative Heading with backtracking (to avoid consuming input on failure)
alternativeHeading :: Parser Block
alternativeHeading = do
  content <- some parseInline -- Parse heading content
  _ <- is '\n' -- Line break
  underline <- some (oneof "=-") -- Parse either '=' or '-'
  _ <- inlineSpace -- Skip trailing whitespace
  let level = if head underline == '=' then 1 else 2
  guard (length underline >= 2) -- Ensure there are at least two '=' or '-'
  return $ Heading level content

-- Parse headings (hash or alternative style)
parseHeading :: Parser Block
parseHeading = hashHeading <|> alternativeHeading

-- Image
parseImage :: Parser Block
parseImage = do
  _ <- is '!'
  _ <- is '['
  alt <- many (noneof "]")
  _ <- is ']'
  _ <- is '('
  url <- many (noneof " ")
  _ <- spaces1
  title <- imageTitleParser
  _ <- is ')'
  return $ Image alt url (fromMaybe "" title)

-- parse the title of image
imageTitleParser :: Parser (Maybe String)
imageTitleParser = do
  _ <- optional (is '"')
  title <- optional (many (noneof "\""))
  _ <- optional (is '"')
  return title

-- Blockquote line parser
parseBlockQuoteLine :: Parser Block
parseBlockQuoteLine = do
  _ <- spaces
  _ <- is '>'
  _ <- optional space
  content <- many parseInline
  _ <- optional (is '\n')
  return $ FreeText content

-- Blockquote parser
parseBlockQuote :: Parser Block
parseBlockQuote = do
  quoteLines <- some parseBlockQuoteLine
  return $ BlockQuote quoteLines

-- Code block parser
parseCode :: Parser Block
parseCode = do
  _ <- spaces
  _ <- string "```"
  lang <- many (noneof "\n")
  _ <- is '\n'
  CodeBlock lang <$> parseCodeContent

-- Parse the code line recursively until the closing tag
-- 解析代码块内容
parseCodeContent :: Parser String
parseCodeContent = do
  line <- many (noneof "\n")
  next <- optional (is '\n')
  end <- optional (string "```")
  case end of
    Just _  -> return line
    Nothing -> do
      rest <- parseCodeContent
      return $ line ++ maybe "" (:[]) next ++ rest

-- Footnote reference parser
parseFootnoteRef :: Parser Block
parseFootnoteRef = do
  _ <- spaces
  _ <- string "[^"
  num <- parsePositiveInt
  _ <- string "]:"
  _ <- spaces
  content <- some (noneof "\n")
  return $ FootnoteRef num content

-- | ----------------------------------------------------------------- |
-- | ------------------------ ordered List --------------------------- |
-- | ----------------------------------------------------------------- |

-- parse a list item of ordered list
-- >>> parse parseOrderedListItem "1. item 1"
-- Result >< ListItem {isSublist = False, liContent = [PlainText "item 1"]}
-- >>> parse parseOrderedListItem "    1. item 2"
-- Result >< ListItem {isSublist = True, liContent = [PlainText "item 2"]}
parseOrderedListItem :: Parser ListItem
parseOrderedListItem = do
  leadingSpaces <- many (is ' ')
  let indentation = length leadingSpaces
  let isSub = indentation == 4 -- Check if the current list item is a sublist
  guard (indentation == 0 || indentation == 4)

  _ <- parsePositiveInt
  _ <- is '.'
  _ <- some (oneof " \t")
  content <- many parseInline
  _ <- optional (is '\n')

  return $ ListItem isSub content

-- Ordered List
parseOrderedList :: Parser Block
parseOrderedList = do
  items <- some parseOrderedListItem
  _ <- optional parseEmptyLines
  return $ OrderedList items


-- Convert ordered list into HTML
-- >>> convertOrderedList [ListItem False [PlainText "item 1"], ListItem True [PlainText "subitem 1"], ListItem False [PlainText "item 2"]]
-- "    <ol>\n        <li>item 1</li>\n        <ol>\n            <li>subitem 1</li>\n        </ol>\n        <li>item 2</li>\n    </ol>\n"
-- >>> convertOrderedList [ListItem False [PlainText "item 1"], ListItem True [PlainText "subitem 1"], ListItem True [PlainText "subitem 2"], ListItem False [PlainText "item 2"]]
-- "    <ol>\n        <li>item 1</li>\n        <ol>\n            <li>subitem 1</li>\n            <li>subitem 2</li>\n        </ol>\n        <li>item 2</li>\n    </ol>\n"

convertOrderedList :: [ListItem] -> String
convertOrderedList items = "    <ol>\n" ++ processItems items (extractSublistRanges items) 0 ++ "    </ol>\n"
  where
    processItems :: [ListItem] -> [(Int, Int)] -> Int -> String
    processItems [] _ _ = ""
    processItems (item : rest) subRanges index =
      let ListItem isSub content = item
          indent = if isSub then "                " else "        " -- 16 spaces indentation for sublist, 12 spaces for non-sublist
          liHtml = indent ++ "<li>" ++ concatMap convertInline content ++ "</li>\n"
          startTags = if any (\(start, _) -> start == index) subRanges then "            <ol>\n" else ""
          endTags = if any (\(_, end) -> end == index) subRanges then "            </ol>\n" else ""
       in startTags ++ liHtml ++ endTags ++ processItems rest subRanges (index + 1)

-- | ----------------------------------------------------------------- |
-- | ---------------------- Unordered List --------------------------- |
-- | ----------------------------------------------------------------- |

-- paese a list item of unordered list
-- >>> parse parseUnorderedListItem "- item 1"
-- Result >< ListItem {isSublist = False, liContent = [PlainText "item 1"]}
-- >>> parse parseUnorderedListItem "    - item 2"
-- Result >< ListItem {isSublist = True, liContent = [PlainText "item 2"]}
-- >>> parse parseUnorderedListItem "- item 3     "
-- Result >< ListItem {isSublist = False, liContent = [PlainText "item 3     "]}
parseUnorderedListItem :: Parser ListItem
parseUnorderedListItem = do
  leadingSpaces <- many (is ' ')
  let indentation = length leadingSpaces
  let isSub = indentation == 4
  guard (indentation == 0 || indentation == 4)

  _ <- oneof "-*+"
  _ <- some (oneof " \t")
  content <- many parseInline
  _ <- optional (is '\n')

  return $ ListItem isSub content

-- parse unordered list
parseUnorderedList :: Parser Block
parseUnorderedList = do
  items <- some parseUnorderedListItem
  _ <- optional parseEmptyLines
  return $ UnorderedList items

-- Convert ordered list into HTML
convertUnorderedList :: [ListItem] -> String
convertUnorderedList items = "    <ul>\n" ++ processItems items (extractSublistRanges items) 0 ++ "    </ul>\n"
  where
    processItems :: [ListItem] -> [(Int, Int)] -> Int -> String
    processItems [] _ _ = ""
    processItems (item : rest) subRanges index =
      let ListItem isSub content = item
          indent = if isSub then "                " else "        " -- 16 spaces for sublist, 8 spaces for non-sublist
          liHtml = indent ++ "<li>" ++ concatMap convertInline content ++ "</li>\n"
          startTags = if any (\(start, _) -> start == index) subRanges then "            <ul>\n" else ""
          endTags = if any (\(_, end) -> end == index) subRanges then "            </ul>\n" else ""
       in startTags ++ liHtml ++ endTags ++ processItems rest subRanges (index + 1)

-- | -------------------------------------------------------- |
-- | ---------------------- Table --------------------------- |
-- | -------------------------------------------------------- |

-- Rewrite all inline text modifiers to exclude the pipe character

-- >>> parse parseItalicNoPipe " _italic_ | rest parts"
-- Result >| rest parts< Italic [PlainText "italic"]
parseItalicNoPipe :: Parser Inline
parseItalicNoPipe = do
  _ <- spaces
  _ <- is '_'
  content <- some (noneof "_\n|") -- exclude pipe '|' character
  _ <- is '_'
  _ <- spaces
  return $ Italic [PlainText content]

parseBoldNoPipe :: Parser Inline
parseBoldNoPipe = do
  _ <- spaces
  _ <- string "**"
  content <- some (noneof "*\n|")
  _ <- string "**"
  _ <- spaces
  return $ Bold [PlainText content]

parseStrikethroughNoPipe :: Parser Inline
parseStrikethroughNoPipe = do
  _ <- spaces
  _ <- string "~~"
  content <- some (noneof "~\n|")
  _ <- string "~~"
  _ <- spaces
  return $ Strikethrough [PlainText content]

parseLinkNoPipe :: Parser Inline
parseLinkNoPipe = do
  _ <- spaces
  _ <- is '['
  linkText <- some (noneof "]\n|")
  _ <- is ']'
  _ <- is '('
  url <- some (noneof ")\n| ")
  _ <- is ')'
  _ <- spaces
  return $ Link url [PlainText linkText]

parseInlineCodeNoPipe :: Parser Inline
parseInlineCodeNoPipe = do
  _ <- spaces
  _ <- is '`'
  content <- some (noneof "`\n|")
  _ <- is '`'
  _ <- spaces
  return $ InlineCode content

parseFootnoteInlineNoPipe :: Parser Inline
parseFootnoteInlineNoPipe = do
  _ <- spaces
  _ <- string "[^"
  number <- parsePositiveInt
  _ <- is ']'
  _ <- spaces
  return $ FootnoteInline number

parsePlainTextNoPipe :: Parser Inline
parsePlainTextNoPipe = do
  _ <- spaces
  text <- some (noneof "_*~`[^|\n")
  _ <- spaces
  return $ PlainText text

-- >>>parse parseInlineNoPipe " hello | world "
-- Result >| world < PlainText "hello"
parseInlineNoPipe :: Parser Inline
parseInlineNoPipe = do
  parseItalicNoPipe
    <|> parseBoldNoPipe
    <|> parseStrikethroughNoPipe
    <|> parseLinkNoPipe
    <|> parseInlineCodeNoPipe
    <|> parseFootnoteInlineNoPipe
    <|> parsePlainTextNoPipe

-- Parse table row
-- >>> parse parseTableRow "  | _a_ | **b** | ~~c ~~   |"
-- Result >< TableRow [Italic [PlainText "a"],Bold [PlainText "b"],Strikethrough [PlainText "c "]]
parseTableRow :: Parser TableRow
parseTableRow = do
  _ <- spaces
  _ <- is '|'
  content <- sepBy1 parseInlineNoPipe (is '|')
  _ <- is '|'
  return $ TableRow content

-- Parse separator row
-- >>> parse parseSeparatorRow "  |---|---|---|"
-- Result >< TableRow [PlainText "---"]

parseSeparatorRow :: Parser TableRow
parseSeparatorRow = do
  _ <- spaces
  _ <- is '|'
  _ <- sepBy1 parseSeparatorRowCell (is '|')
  _ <- is '|'
  return $ TableRow [PlainText ""]

-- parse separator row in one cell
-- >>> parse parseSeparatorRowCell " ---"
-- Result >< PlainText "---"

parseSeparatorRowCell :: Parser Inline
parseSeparatorRowCell = do
  _ <- spaces
  _ <- string "---" -- ensure that the cell contains at least 3 dashes
  content <- many (oneof "-")
  _ <- spaces
  return $ PlainText (content ++ "---")

-- Parse table
-- >>> parse parseTable "  | a | b | c |\n  |---|---|---|\n  | _a_ | **b** | ~~c~~ |\n | a | b | c |"
-- Result >< Table (TableRow [PlainText "a ",PlainText "b ",PlainText "c "]) [TableRow [Italic [PlainText "a"],Bold [PlainText "b"],Strikethrough [PlainText "c"]],TableRow [PlainText "a ",PlainText "b ",PlainText "c "]]
parseTable :: Parser Block
parseTable = do
  tableHeader <- parseTableRow
  _ <- parseSeparatorRow
  rows <- some parseTableRow
  _ <- optional parseEmptyLines
  return $ Table tableHeader rows

-- convert table to HTML
convertTable :: TableRow -> [TableRow] -> String
convertTable header rows =
  "    <table>\n"
    ++ "        <thead>\n"
    ++ convertTableHeader header
    ++ "        </thead>\n"
    ++ "        <tbody>\n"
    ++ concatMap convertTableRow rows
    ++ "        </tbody>\n"
    ++ "    </table>\n"

-- convert table header to HTML
convertTableHeader :: TableRow -> String
convertTableHeader (TableRow cells) =
  "            <tr>\n"
    ++ concatMap convertTableHeaderCell cells
    ++ "            </tr>\n"

-- convert table header cell to HTML
convertTableHeaderCell :: Inline -> String
convertTableHeaderCell cell =
  "                <th>" ++ trimEnd (convertInline cell) ++ "</th>\n"

-- convert table row to HTML
convertTableRow :: TableRow -> String
convertTableRow (TableRow cells) =
  "            <tr>\n"
    ++ concatMap convertTableCell cells
    ++ "            </tr>\n"

-- convert table cell to HTML
convertTableCell :: Inline -> String
convertTableCell cell =
  "                <td>" ++ trimEnd (convertInline cell) ++ "</td>\n"

-- | -----------------------------------------------------
-- | -------------- Parsing and Conversion ---------------
-- | -----------------------------------------------------
parseBlock :: Parser Block
parseBlock = do
  _ <- optional parseEmptyLines
  block <-
    parseTable
      <|> parseFootnoteRef
      <|> parseUnorderedList
      <|> parseOrderedList
      <|> parseHeading
      <|> parseBlockQuote
      <|> parseCode
      <|> parseImage
      <|> parseFreeText
  _ <- optional parseEmptyLines
  return block

-- Parse multiple blocks
markdownParser :: Parser ADT
markdownParser = do
  _ <- optional parseEmptyLines
  blocks <- some parseBlock
  return $ ADT blocks

-- Convert block-level elements to HTML
convertBlock :: Block -> String
convertBlock (Table header rows) = convertTable header rows
convertBlock (OrderedList items) = convertOrderedList items
convertBlock (UnorderedList items) = convertUnorderedList items
convertBlock (Heading level inlines) = convertHeading level inlines
convertBlock (FreeText inlines) = convertFreeText inlines
convertBlock (FootnoteRef num content) = convertFootnoteRef num content
convertBlock (Image alt url title) = convertImage alt url title
convertBlock (BlockQuote blocks) = convertBlockQuote blocks
convertBlock (CodeBlock lang code) = convertCodeBlock lang code

-- Convert Heading to HTML
convertHeading :: Int -> [Inline] -> String
convertHeading level inlines =
  let tag = "h" ++ show level
   in "    <" ++ tag ++ ">" ++ concatMap convertInline inlines ++ "</" ++ tag ++ ">\n"

-- Convert FreeText to HTML
convertFreeText :: [Inline] -> String
convertFreeText inlines =
  "    <p>" ++ concatMap convertInline inlines ++ "</p>\n"

-- Convert FootnoteRef to HTML
convertFootnoteRef :: Int -> String -> String
convertFootnoteRef num content =
  "    <p id=\"fn" ++ show num ++ "\">" ++ content ++ "</p>\n"

-- Convert Image to HTML
convertImage :: String -> String -> String -> String
convertImage alt url title =
  "    <img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\">\n"

-- Convert BlockQuote to HTML
convertBlockQuote :: [Block] -> String
convertBlockQuote blocks =
  "    <blockquote>\n" ++ concatMap convertBlockQuoteLine blocks ++ "    </blockquote>\n"

-- Convert CodeBlock to HTML
convertCodeBlock :: String -> String -> String
convertCodeBlock lang code =
  let codeClass = if null lang then "" else " class=\"language-" ++ lang ++ "\""
      trimmedCode = trimEnd code
   in "    <pre><code" ++ codeClass ++ ">" ++ trimmedCode ++ "</code></pre>\n"

-- Convert blockquote line to HTML
convertBlockQuoteLine :: Block -> String
convertBlockQuoteLine (FreeText inlines) =
  "        <p>" ++ concatMap convertInline inlines ++ "</p>\n"
convertBlockQuoteLine _ = ""


-- Convert ADT to a full HTML page with standard HTML structure and indentation
convertADTHTML :: ADT -> String
convertADTHTML (ADT blocks) =
  "<!DOCTYPE html>\n<html lang=\"en\">\n\n"
    ++ "<head>\n"
    ++ "    <meta charset=\"UTF-8\">\n"
    ++ "    <title>Test</title>\n"
    ++ "</head>\n\n"
    ++ "<body>\n"
    ++ concatMap convertBlock blocks
    ++ "</body>\n\n"
    ++ "</html>\n"

-- | -----------------------------------------------------
-- | ------------------ Utility Functions ----------------
-- | -----------------------------------------------------

-- sepBy1 function
-- p is the parser for the element
-- sep is the parser for the separator
-- This parser will parse one or more elements separated by the separator

-- >>> parse (sepBy1 parseInlineNoPipe (is '|')) "_a_|~~a~~|a|"
-- Result >|< [Italic [PlainText "a"],Strikethrough [PlainText "a"],PlainText "a"]
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

-- Substract sublist ranges
-- For example:
-- For a list with 8 ListItem(index from 0 - 7), this function will return the the range of the sublist index(list of tuples)
-- >>> extractSublistRanges [ListItem False [PlainText "item 1"], ListItem True [PlainText "subitem 1"], ListItem True [PlainText "subitem 2"], ListItem False [PlainText "item 2"]]
-- [(1,2)]
-- >>>
extractSublistRanges :: [ListItem] -> [(Int, Int)]
extractSublistRanges items = go items 0 [] Nothing
  where
    -- Recursive function with current index, result set, and optional start index
    go :: [ListItem] -> Int -> [(Int, Int)] -> Maybe Int -> [(Int, Int)]
    go [] _ ranges Nothing = ranges -- No more items and open sublist group
    go [] _ ranges (Just start) = ranges ++ [(start, start)] -- only one item in the sublist group
    go (item : rest) index ranges currentStart
      | isSublist item = case currentStart of
          Nothing -> go rest (index + 1) ranges (Just index) -- start new sublist group
          Just start -> go rest (index + 1) ranges (Just start)
      | otherwise = case currentStart of
          Nothing -> go rest (index + 1) ranges Nothing
          Just start -> go rest (index + 1) (ranges ++ [(start, index - 1)]) Nothing -- close the sublist group


-- Remove trailing whitespaces from a string
-- >>> trimEnd "hello   "
-- "hello"
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse
