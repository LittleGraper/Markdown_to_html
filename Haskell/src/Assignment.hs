module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (inlineSpace, is, noneof, oneof, parseEmptyLines, parsePositiveInt, spaces, string)

-- Define Algebraic Data Types(ADTs)
data ADT = Document [Block]
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
  = Heading Int [Inline] -- 标题，包含级别和行内元素
  | FreeText [Inline] -- 段落，包含行内元素
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

-- 转换行内元素为 HTML
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
  _ <- optional parseEmptyLines
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

-- parse all block-level elements
parseBlock :: Parser Block
parseBlock = do
  _ <- optional parseEmptyLines
  block <- parseHeading <|> parseFreeText
  _ <- optional parseEmptyLines
  return block

-- Parse multiple blocks
markdownParser :: Parser ADT
markdownParser = do
  _ <- optional parseEmptyLines
  blocks <- some parseBlock
  return $ Document blocks

-- Convert block-level elements to HTML
convertBlock :: Block -> String
convertBlock (Heading level inlines) =
  let tag = "h" ++ show level
   in "<" ++ tag ++ ">" ++ concatMap convertInline inlines ++ "</" ++ tag ++ ">\n"
convertBlock (FreeText inlines) =
  "<p>" ++ concatMap convertInline inlines ++ "</p>\n"

-- Convert ADT to full HTML page
convertADTHTML :: ADT -> String
convertADTHTML (Document blocks) = concatMap convertBlock blocks

-- Get current time (unused in this example)
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime
