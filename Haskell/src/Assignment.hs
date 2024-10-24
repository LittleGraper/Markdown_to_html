module Assignment (markdownParser, convertADTHTML) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, noneof, string, parsePositiveInt)
import           Control.Applicative (Alternative (..))



-- data ADT = Document [Block]
--   deriving (Show, Eq) 


data ADT = InlineContent [Inline]
  deriving (Show, Eq)

data Inline
    = PlainText String
    | Italic [Inline]
    | Bold [Inline]
    | Strikethrough [Inline]
    | Link String [Inline]
    | InlineCode String
    | FootnoteInline Int
    deriving (Show, Eq)

-- Parsers returning Parser Inline

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
parseInline = parseItalic
          <|> parseBold
          <|> parseStrikethrough
          <|> parseLink
          <|> parseInlineCode
          <|> parseFootnoteInline
          <|> parsePlainText

markdownParser :: Parser ADT
markdownParser = do
  inlines <- many parseInline
  return $ InlineContent inlines



-- 转换行内元素为 HTML
convertInline :: Inline -> String
convertInline (PlainText text) = text
convertInline (Italic inlines) = "<em>" ++ concatMap convertInline inlines ++ "</em>"
convertInline (Bold inlines) = "<strong>" ++ concatMap convertInline inlines ++ "</strong>"
convertInline (Strikethrough inlines) = "<del>" ++ concatMap convertInline inlines ++ "</del>"
convertInline (Link url inlines) = "<a href=\"" ++ url ++ "\">" ++ concatMap convertInline inlines ++ "</a>"
convertInline (InlineCode code) = "<code>" ++ code ++ "</code>"
convertInline (FootnoteInline n) = "<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>"








-- 转换 ADT 为完整的 HTML 页面
-- 将 ADT 转换为 HTML
convertADTHTML :: ADT -> String
convertADTHTML (InlineContent inlines) = concatMap convertInline inlines


getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime