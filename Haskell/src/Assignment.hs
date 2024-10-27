{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Instances (Parser (..))
import Parser (inlineSpace, is, noneof, oneof, parseEmptyLines, parsePositiveInt, space, spaces, spaces1, string)

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
  deriving (
            Show, Eq)

-- Define list items for ordered lists
-- including the indentation level and content
data ListItem = ListItem
  { isSublist :: Bool, -- indicate if the current list item is a sublist or not
    liContent :: [Inline] -- list item content
  }
  deriving (Show, Eq)

-- | --------------------------------------------------
-- | --------------- Text Modifiers -------------------
-- | --------------------------------------------------
parseItalic :: Parser Inline
parseItalic = do
  _ <- is '_'
  content <- some (noneof "_\n")
  _ <- is '_'
  return $ Italic [PlainText content]

parseBold :: Parser Inline
parseBold = do
  _ <- string "**"
  content <- some (noneof "*\n")
  _ <- string "**"
  return $ Bold [PlainText content]

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
  return $ Image alt url (maybe "" id title)

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
parseCodeContent :: Parser String
parseCodeContent = do
  line <- many (noneof "\n")
  _ <- is '\n'
  rest <- (string "```" >> return "") <|> parseCodeContent
  return $ line ++ "\n" ++ rest

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

--------------------------------------------------
----------------- Ordered List -------------------
--------------------------------------------------

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
          indent = if isSub then "            " else "        " -- 12 spaces indentation for sublist, 8 spaces for non-sublist
          liHtml = indent ++ "<li>" ++ concatMap convertInline content ++ "</li>\n"
          startTags = if any (\(start, _) -> start == index) subRanges then "        <ol>\n" else ""
          endTags = if any (\(_, end) -> end == index) subRanges then "        </ol>\n" else ""
       in startTags ++ liHtml ++ endTags ++ processItems rest subRanges (index + 1)


-- Substract sublist ranges
-- For example: 
-- For a list with 8 ListItem(index from 0 - 7), this function will return the the range of the sublist index(list of tuples)
-- >>> extractSublistRanges [ListItem False [PlainText "item 1"], ListItem True [PlainText "subitem 1"], ListItem True [PlainText "subitem 2"], ListItem False [PlainText "item 2"]]
-- [(1,2)]
-- >>> 
extractSublistRanges :: [ListItem] -> [(Int, Int)]
extractSublistRanges items = go items 0 [] Nothing
  where
    -- 递归函数，带有当前索引、结果集和可选的开始索引
    go :: [ListItem] -> Int -> [(Int, Int)] -> Maybe Int -> [(Int, Int)]
    go [] _ ranges Nothing = ranges -- 没有更多的项且没有开放的子列表组
    go [] _ ranges (Just start) = ranges ++ [(start, start)] -- 子列表组只有一个项
    go (item : rest) index ranges currentStart
      | isSublist item = case currentStart of
          Nothing -> go rest (index + 1) ranges (Just index) -- 开始新的子列表组
          Just start -> go rest (index + 1) ranges (Just start) -- 继续在当前子列表组
      | otherwise = case currentStart of
          Nothing -> go rest (index + 1) ranges Nothing -- 不是子列表项，继续
          Just start -> go rest (index + 1) (ranges ++ [(start, index - 1)]) Nothing -- 关闭子列表组


--------------------------------------------------
----------------- Unordered List -----------------
--------------------------------------------------

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
          indent = if isSub then "            " else "        " -- 12 spaces indentation for sublist, 8 spaces for non-sublist
          liHtml = indent ++ "<li>" ++ concatMap convertInline content ++ "</li>\n"
          startTags = if any (\(start, _) -> start == index) subRanges then "        <ul>\n" else ""
          endTags = if any (\(_, end) -> end == index) subRanges then "        </ul>\n" else ""
       in startTags ++ liHtml ++ endTags ++ processItems rest subRanges (index + 1)




















-- | -----------------------------------------------------
-- | -------------- Parsing and Conversion ---------------
-- | -----------------------------------------------------
parseBlock :: Parser Block
parseBlock = do
  _ <- optional parseEmptyLines
  block <-
    parseFootnoteRef
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
convertBlock (OrderedList items) = convertOrderedList items
convertBlock (UnorderedList items) = convertUnorderedList items
convertBlock (Heading level inlines) =
  let tag = "h" ++ show level
   in "    <" ++ tag ++ ">" ++ concatMap convertInline inlines ++ "</" ++ tag ++ ">\n"
convertBlock (FreeText inlines) =
  "    <p>" ++ concatMap convertInline inlines ++ "</p>\n"
convertBlock (FootnoteRef num content) =
  "    <p id=\"fn" ++ show num ++ "\">" ++ content ++ "</p>\n"
convertBlock (Image alt url title) =
  "    <img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\">\n"
convertBlock (BlockQuote blocks) =
  "    <blockquote>\n" ++ concatMap convertBlockQuoteLine blocks ++ "    </blockquote>\n"
convertBlock (CodeBlock lang code) =
  let codeClass = if null lang then "" else " class=\"language-" ++ lang ++ "\""
   in "    <pre><code" ++ codeClass ++ ">" ++ code ++ "</code></pre>\n"

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
