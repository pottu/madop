-- FIXME: Don't expose all functions.
module Parser where

import Types
import Control.Applicative ((<|>))
import qualified Text.Parsec as Prsc
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Map.Strict as Map

data Context = InParagraph 
             | NotInParagraph
             deriving (Eq, Show)

type LinkDef = (String, Maybe String)

type ParserState = (Context, Map.Map String LinkDef)

type Parser = Prsc.Parsec String ParserState

mdSymbols = ['\\', '-', '*', '_', '[', ']', '(', ')', '#', '`', '!']

-- https://www.w3schools.com/html/html_blocks.asp
htmlBlocks = 
  ["address", "article", "aside", "blockquote", "canvas", "dd", "div", "dl",
  "dt", "fieldset", "figcaption", "figure", "footer", "form", "h1", "h2", "h3",
  "h4", "h5", "h6", "header", "hr", "li", "main", "nav", "noscript", "ol", "p",
  "pre", "section", "table", "tfoot", "ul", "video"]

htmlSpans =
  ["a", "abbr", "acronym", "b", "bdo", "big", "br", "button", "cite", "code",
  "dfn", "em", "i", "img", "input", "kbd", "label", "map", "object", "output",
  "q", "samp", "script", "select", "small", "span", "strong", "sub", "sup",
  "textarea", "time", "tt", "var"]




consumeRefDefs :: Parser String 
consumeRefDefs = unlines <$> Prsc.manyTill (Prsc.try refDef <|> anyLine) Prsc.eof
    where
      anyLine :: Parser String
      anyLine = Prsc.manyTill Prsc.anyChar Prsc.endOfLine

      -- FIXME: Title allowed on new line
      refDef :: Parser String
      refDef = do
        Prsc.count 3 (Prsc.optional $ Prsc.char ' ')
        ref <- map toLower <$> parseTextBetween '[' ']' (Prsc.noneOf "\n")
        Prsc.char ':'
        Prsc.skipMany1 $ Prsc.char ' '
        link <- parseTextBetween '<' '>' (Prsc.noneOf "\n")
            <|> Prsc.many1 (Prsc.notFollowedBy Prsc.space *> Prsc.anyChar)
        Prsc.skipMany $ Prsc.char ' '
        title <- Prsc.optionMaybe $ parseTextBetween '"' '"'   (Prsc.noneOf "\n")
                                <|> parseTextBetween '\'' '\'' (Prsc.noneOf "\n")
                                <|> parseTextBetween '(' ')'   (Prsc.noneOf "\n")
        blankline

        (ctx, map) <- Prsc.getState
        Prsc.putState (ctx, Map.insert ref (link, title) map)
        return ""



-- | Parse a string formatted with Markdown.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.runParser parseDocument (NotInParagraph, Map.empty) "" (s ++ "\n")
             in either (error . show) id parsed



blankline :: Parser Char
blankline = Prsc.try $ (Prsc.skipMany (Prsc.char ' ')) *> Prsc.endOfLine

blanklines :: Parser ()
blanklines = Prsc.skipMany blankline


parseDocument :: Parser Document
parseDocument = do
  input <- consumeRefDefs
  Prsc.setInput input
  blanklines *> Prsc.manyTill parseBlock Prsc.eof



parseBlock :: Parser Block
parseBlock = Prsc.choice [
             Prsc.try parseHeader
           , Prsc.try parseCodeBlock
           , Prsc.try parseBlockQuote
           , Prsc.try parseHtmlBlock
           , Prsc.try parseHorizontalRule
           , parseParagraph
           ] <* blanklines


parseHeader :: Parser Block
parseHeader = Prsc.try atxHeader <|> setextHeader
  where
    atxHeader :: Parser Block
    atxHeader = do
      level <- length <$> Prsc.many1 (Prsc.char '#')
      Prsc.many1 Prsc.space
      header <- Prsc.manyTill parseSpan (Prsc.try ending)
      return $ Header level header 
      where 
        ending = do
          Prsc.skipMany (Prsc.char ' ')
          Prsc.skipMany (Prsc.char '#')
          blankline

    setextHeader :: Parser Block
    setextHeader = do
      header <- Prsc.many1 parseSpan
      Prsc.endOfLine
      c <- head <$> (Prsc.many1 (Prsc.char '=') <|> Prsc.many1 (Prsc.char '-'))
      Prsc.endOfLine
      case c of
        '=' -> return $ Header 1 header
        '-' -> return $ Header 2 header



parseParagraph :: Parser Block
parseParagraph = do
  map <- snd <$> Prsc.getState
  Prsc.putState (InParagraph, map)
  spans <- Prsc.many1 parseSpan
  blankline
  Prsc.putState (NotInParagraph, map)
  return $ Paragraph spans



parseCodeBlock :: Parser Block
parseCodeBlock = do
  content <- Prsc.many1 codeLine
  blankline
  return $ CodeBlock content
    where
      codeLine = (Prsc.count 4 (Prsc.char ' ') <|> Prsc.string "\t")
              *> Prsc.manyTill Prsc.anyChar Prsc.endOfLine



parseHorizontalRule :: Parser Block
parseHorizontalRule = do
  Prsc.skipMany $ Prsc.char ' '
  opening <- Prsc.oneOf "*-_"
  Prsc.count 2 ((Prsc.skipMany (Prsc.char ' ')) *> Prsc.char opening)
  Prsc.skipMany $ Prsc.char ' ' <|> Prsc.char opening
  Prsc.endOfLine
  return HorizontalRule


-- Note: doesn't enforce blanklines around block
parseHtmlBlock :: Parser Block
parseHtmlBlock = do
  Prsc.char '<'
  tag <- Prsc.many1 Prsc.letter
  if tag `notElem` htmlBlocks
    then Prsc.unexpected "Not a valid HTML-tag."     
    else do
      let closing = "</" ++ tag ++ ">"
      block <- Prsc.many $ Prsc.notFollowedBy (Prsc.string closing) *> Prsc.anyChar 
      Prsc.string closing 
      blankline
      return $ HtmlBlock $ "<" ++ tag ++ block ++ closing
    

parseBlockQuote :: Parser Block  
parseBlockQuote = do
  let quoteStart = Prsc.try $ Prsc.string "> "
  let quoteLine = (Prsc.many (Prsc.notFollowedBy Prsc.endOfLine *> Prsc.anyChar)) <> (Prsc.string "\n")
  quote <- concat <$> (Prsc.many1 $ quoteStart *> quoteLine)
  input <- Prsc.getInput
  Prsc.setInput (quote ++ "\n")
  blocks <- Prsc.manyTill parseBlock Prsc.eof
  Prsc.setInput input
  return $ BlockQuote blocks



parseSpan :: Parser Span
parseSpan = Prsc.try parseNl
        <|> Prsc.try parseLineBreak
        <|> Prsc.try parseSpace
        <|> Prsc.try parseImage
        <|> Prsc.try parseLink
        <|> Prsc.try parseStrong
        <|> Prsc.try parseEmph
        <|> Prsc.try parseCode
        <|> parseText
        <|> parseSymbol



parseLineBreak :: Parser Span
parseLineBreak = do
  Prsc.count 2 $ Prsc.char ' '
  Prsc.endOfLine
  return LineBreak



parseNl :: Parser Span
parseNl = do
  state <- fst <$> Prsc.getState
  case state of
    InParagraph -> do
      Prsc.endOfLine
      Prsc.notFollowedBy blankline
      Prsc.notFollowedBy parseHeader
      Prsc.notFollowedBy parseHorizontalRule
      Prsc.notFollowedBy parseHtmlBlock
      return SoftBreak 
    _ -> Prsc.unexpected "Rule only applies in paragraph"



parseSymbol :: Parser Span
parseSymbol = do
  c <- Prsc.oneOf mdSymbols
  if c == '\\'
    then do
      c <- Prsc.option '\\' (Prsc.oneOf mdSymbols)
      return $ Text [c]
    else return $ Text [c]



parseText :: Parser Span
parseText = do
  text <- Prsc.many1 $ Prsc.noneOf (' ':'\n':mdSymbols)
  return $ Text text



parseSpace :: Parser Span
parseSpace = do
  Prsc.char ' '
  return Space



parseTextBetween :: Char -> Char -> Parser a -> Parser [a]
parseTextBetween open close p = do
  Prsc.char open
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char close) *> p
  Prsc.char close
  return content



parseChar :: Parser Char 
parseChar = Prsc.noneOf "\n" <|> parseNl *> return ' '



-- TODO: Handle reference-style links.
parseLink :: Parser Span 
parseLink = Prsc.try inlineLink <|> autoLink <|> refLink
  where
    inlineLink :: Parser Span
    inlineLink = do
      text <- parseTextBetween '[' ']' parseChar
      Prsc.char '('
      href <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char ')' <|> Prsc.char ' ')
                        *> parseChar
      Prsc.skipMany $ Prsc.char ' '
      title <- Prsc.optionMaybe $ parseTextBetween '"' '"' parseChar
      Prsc.char ')'
      return $ Link text href title

    -- FIXME: This is wrong. Actually parse an email, otherwise
    -- default to link.
    -- FIXME: Doesn't encode emails
    autoLink :: Parser Span
    autoLink = Prsc.try email <|> link
        where
          email :: Parser Span
          email = do
            Prsc.char '<'
            Email <$> Prsc.many1 (Prsc.notFollowedBy (Prsc.char '@') *> parseChar)
                   <> Prsc.string "@"
                   <> Prsc.manyTill parseChar (Prsc.char '>')

          link :: Parser Span
          link = do
            link <- parseTextBetween '<' '>' parseChar
            return $ Link link link Nothing

    refLink :: Parser Span
    refLink = do
      text <- parseTextBetween '[' ']' parseChar
      Prsc.optional $ Prsc.char ' '
      -- Reference is either implicit or explicit
      ref <- Prsc.try (Prsc.string "[]" *> return (map toLower text))
         <|> map toLower <$> parseTextBetween '[' ']' parseChar
      refs <- snd <$> Prsc.getState
      case Map.lookup ref refs of
        Just (link, title) -> return $ Link text link title
        Nothing -> Prsc.unexpected "Link reference not found."






-- Shares almost all code with parseLink
-- TODO: Handle reference-style images.
parseImage :: Parser Span
parseImage = do
  Prsc.char '!'
  alt <- parseTextBetween '[' ']' parseChar
  Prsc.char '('
  path <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char ')' <|> Prsc.char ' ')
                    *> parseChar
  Prsc.skipMany $ Prsc.char ' '
  title <- Prsc.optionMaybe $ parseTextBetween '"' '"' parseChar
  Prsc.char ')'
  return $ Image path alt title
  
  

-- FIXME: Spec has rules on spaces around opening/closing signs
parseEmph :: Parser Span
parseEmph = do
  opening <- Prsc.char '*' <|> Prsc.char '_'
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char opening) *> parseSpan
  Prsc.char opening
  return $ Emph content



parseStrong :: Parser Span
parseStrong = do
  opening <- Prsc.string "**" <|> Prsc.string "__"
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.string opening) *> parseSpan
  Prsc.string opening
  return $ Strong content



parseCode :: Parser Span
parseCode = do
  opening <- Prsc.many1 (Prsc.char '`') <* Prsc.optional (Prsc.char ' ')
  let closing = (Prsc.optional (Prsc.char ' ')) *> Prsc.string opening
  content <- Prsc.many1 $ Prsc.notFollowedBy closing *> parseChar
  closing
  return $ Code content

    
