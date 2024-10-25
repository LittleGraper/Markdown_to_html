{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (inlineSpace, is, noneof, oneof, parseEmptyLines, parsePositiveInt, spaces, string, spaces1, space)

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
  = Heading Int [Inline]
  | FreeText [Inline]
  | FootnoteRef Int String
  | Image String String String   -- alt, url, title
  | BlockQuote [Block]
  | CodeBlock String String

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


-- Image
-- >>> parse parseImage ""
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
-- This parser try to parse the title of image until it meets the '""' in the end of the title.
imageTitleParser :: Parser (Maybe String)
imageTitleParser = do
  _ <- optional (is '"')      
  title <- optional (many (noneof "\"")) 
  _ <- optional (is '"')       
  return title



-- Quote line parser
parseBlockQuoteLine :: Parser Block
parseBlockQuoteLine = do
  _ <- spaces
  _ <- is '>'
  _ <- space
  content <- many parseInline
  _ <- spaces
  _ <- optional (is '\n')
  return $ FreeText content

-- Quote block parser
parseBlockQuote :: Parser Block
parseBlockQuote = do
  blockQuote <- some parseBlockQuoteLine
  _ <- optional parseEmptyLines
  return $ BlockQuote blockQuote



-- 辅助函数：解析代码内容直到遇到结束的反引号行
parseCodeLines :: Parser String
parseCodeLines = do
  line <- many (noneof "\n")  -- 解析直到行尾
  _ <- is '\n'                -- 消耗行尾换行符
  rest <- optional parseCodeLines  -- 递归解析其余内容
  return $ line ++ "\n" ++ maybe "" id rest

-- 代码块解析器
parseCode :: Parser Block
parseCode = do
  _ <- spaces
  _ <- string "```"                  -- 开始标记
  lang <- optional (many (noneof "\n")) -- 可选的语言标识符
  _ <- is '\n'
  codeLines <- parseCodeLines         -- 解析代码内容直到结束标记
  _ <- string "```"                  -- 结束标记
  _ <- spaces
  _ <- optional parseEmptyLines
  return $ CodeBlock (maybe "" id lang) codeLines



-- Footnote reference parser
parseFootnoteRef :: Parser Block
parseFootnoteRef = do
  _ <- spaces                     -- 忽略前导空白
  _ <- string "[^"                -- 脚注开头
  num <- parsePositiveInt         -- 脚注编号
  _ <- string "]:"                -- 脚注编号后的分隔符
  _ <- spaces                     -- 忽略冒号后的空白
  content <- some (noneof "\n")   -- 解析内容直到换行
  return $ FootnoteRef num content

-- parse all block-level elements
parseBlock :: Parser Block
parseBlock = do
  _ <- optional parseEmptyLines
  block <- parseFootnoteRef
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
  return $ Document blocks

-- Convert block-level elements to HTML
convertBlock :: Block -> String
convertBlock (Heading level inlines) =
  let tag = "h" ++ show level
   in "<" ++ tag ++ ">" ++ concatMap convertInline inlines ++ "</" ++ tag ++ ">\n"
convertBlock (FreeText inlines) =
  "<p>" ++ concatMap convertInline inlines ++ "</p>\n"
convertBlock (FootnoteRef num content) =
  "<p id=\"fn" ++ show num ++ "\">" ++ content ++ "</p>\n"
convertBlock (Image altText url title) =
  "<img src=\"" ++ url ++ "\" alt=\"" ++ altText ++ "\" title=\"" ++ title ++ "\">\n"
convertBlock (BlockQuote blocks) =
  "<blockquote>\n" ++ concatMap convertBlockQuoteLine blocks ++ "</blockquote>\n"
-- Convert code block to HTML
convertBlock (CodeBlock lang code) =
  let codeClass = if null lang then "" else " class=\"language-" ++ lang ++ "\""
  in "<pre><code" ++ codeClass ++ ">" ++ escapeHtml code ++ "</code></pre>\n"


-- Escape HTML special characters for code blocks
escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c   = [c]

convertBlockQuoteLine :: Block -> String
convertBlockQuoteLine (FreeText inlines) =
  "  <p>" ++ concatMap convertInline inlines ++ "</p>\n"
convertBlockQuoteLine _ = ""

-- Convert ADT to full HTML page
convertADTHTML :: ADT -> String
convertADTHTML (Document blocks) = concatMap convertBlock blocks

-- Get current time (unused in this example)
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime
