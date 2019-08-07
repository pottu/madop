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





consumeRefDefs :: Parser String 
consumeRefDefs = unlines <$> Prsc.manyTill (Prsc.try refDef <|> anyLine) Prsc.eof
    where
      anyLine :: Parser String
      anyLine = Prsc.manyTill Prsc.anyChar Prsc.endOfLine

      -- FIXME: Title allowed on new line
      refDef :: Parser String
      refDef = do
        Prsc.count 3 (Prsc.optional $ Prsc.char ' ')
        ref <- map toLower <$> textBetween '[' ']' (Prsc.noneOf "\n")
        Prsc.char ':'
        Prsc.skipMany1 $ Prsc.char ' '
        link <- textBetween '<' '>' (Prsc.noneOf "\n")
            <|> Prsc.many1 (Prsc.notFollowedBy Prsc.space *> Prsc.anyChar)
        Prsc.skipMany $ Prsc.char ' '
        title <- Prsc.optionMaybe $ textBetween '"' '"'   (Prsc.noneOf "\n")
                                <|> textBetween '\'' '\'' (Prsc.noneOf "\n")
                                <|> textBetween '(' ')'   (Prsc.noneOf "\n")
        blankline

        (ctx, map) <- Prsc.getState
        Prsc.putState (ctx, Map.insert ref (link, title) map)
        return ""



-- | Parse a string formatted with Markdown.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.runParser document (NotInParagraph, Map.empty) "" (s ++ "\n")
             in either (error . show) id parsed



blankline :: Parser Char
blankline = Prsc.try $ (Prsc.skipMany (Prsc.char ' ')) *> Prsc.endOfLine

blanklines :: Parser ()
blanklines = Prsc.skipMany blankline


document :: Parser Document
document = do
  input <- consumeRefDefs
  Prsc.setInput input
  blanklines *> Prsc.manyTill block Prsc.eof



block :: Parser Block
block = Prsc.choice [
             Prsc.try header
           , Prsc.try codeBlock
           , Prsc.try blockQuote
           , Prsc.try htmlBlock
           , Prsc.try horizontalRule
           , paragraph
           ] <* blanklines


header :: Parser Block
header = Prsc.try atxHeader <|> setextHeader
  where
    atxHeader :: Parser Block
    atxHeader = do
      level <- length <$> Prsc.many1 (Prsc.char '#')
      Prsc.many1 Prsc.space
      header <- Prsc.manyTill inline (Prsc.try ending)
      return $ Header level header 
      where 
        ending = do
          Prsc.skipMany (Prsc.char ' ')
          Prsc.skipMany (Prsc.char '#')
          blankline

    setextHeader :: Parser Block
    setextHeader = do
      header <- Prsc.many1 inline
      Prsc.endOfLine
      c <- head <$> (Prsc.many1 (Prsc.char '=') <|> Prsc.many1 (Prsc.char '-'))
      Prsc.endOfLine
      case c of
        '=' -> return $ Header 1 header
        '-' -> return $ Header 2 header



paragraph :: Parser Block
paragraph = do
  map <- snd <$> Prsc.getState
  Prsc.putState (InParagraph, map)
  inlines <- Prsc.many1 inline
  blankline
  Prsc.putState (NotInParagraph, map)
  return $ Paragraph inlines



codeBlock :: Parser Block
codeBlock = do
  content <- Prsc.many1 codeLine
  blankline
  return $ CodeBlock content
    where
      codeLine = (Prsc.count 4 (Prsc.char ' ') <|> Prsc.string "\t")
              *> Prsc.manyTill Prsc.anyChar Prsc.endOfLine



horizontalRule :: Parser Block
horizontalRule = do
  Prsc.skipMany $ Prsc.char ' '
  opening <- Prsc.oneOf "*-_"
  Prsc.count 2 ((Prsc.skipMany (Prsc.char ' ')) *> Prsc.char opening)
  Prsc.skipMany $ Prsc.char ' ' <|> Prsc.char opening
  Prsc.endOfLine
  return HorizontalRule


-- Note: doesn't enforce blanklines around block
htmlBlock :: Parser Block
htmlBlock = do
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
    

blockQuote :: Parser Block  
blockQuote = do
  let quoteStart = Prsc.try $ Prsc.string "> "
  let quoteLine = (Prsc.many (Prsc.notFollowedBy Prsc.endOfLine *> Prsc.anyChar)) <> (Prsc.string "\n")
  quote <- concat <$> (Prsc.many1 $ quoteStart *> quoteLine)
  input <- Prsc.getInput
  Prsc.setInput (quote ++ "\n")
  blocks <- Prsc.manyTill block Prsc.eof
  Prsc.setInput input
  return $ BlockQuote blocks



inline :: Parser Inline
inline = Prsc.try newline
        <|> Prsc.try lineBreak
        <|> Prsc.try space
        <|> Prsc.try image
        <|> Prsc.try link
        <|> Prsc.try strong
        <|> Prsc.try emphasis
        <|> Prsc.try codeSpan
        <|> plaintext
        <|> symbol



lineBreak :: Parser Inline
lineBreak = do
  Prsc.count 2 $ Prsc.char ' '
  Prsc.endOfLine
  return LineBreak



newline :: Parser Inline
newline = do
  state <- fst <$> Prsc.getState
  case state of
    InParagraph -> do
      Prsc.endOfLine
      Prsc.notFollowedBy blankline
      Prsc.notFollowedBy header
      Prsc.notFollowedBy horizontalRule
      Prsc.notFollowedBy htmlBlock
      return SoftBreak 
    _ -> Prsc.unexpected "Rule only applies in paragraph"



symbol :: Parser Inline
symbol = do
  c <- Prsc.oneOf mdSymbols
  if c == '\\'
    then do
      c <- Prsc.option '\\' (Prsc.oneOf mdSymbols)
      return $ Text [c]
    else return $ Text [c]



plaintext :: Parser Inline
plaintext = do
  text <- Prsc.many1 $ Prsc.noneOf (' ':'\n':mdSymbols)
  return $ Text text



space :: Parser Inline
space = do
  Prsc.char ' '
  return Space



textBetween :: Char -> Char -> Parser a -> Parser [a]
textBetween open close p = do
  Prsc.char open
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char close) *> p
  Prsc.char close
  return content



charInBlock :: Parser Char 
charInBlock = Prsc.noneOf "\n" <|> newline *> return ' '



-- TODO: Handle reference-style links.
link :: Parser Inline 
link = Prsc.try inlineLink <|> autoLink <|> refLink
  where
    inlineLink :: Parser Inline
    inlineLink = do
      text <- textBetween '[' ']' charInBlock
      Prsc.char '('
      href <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char ')' <|> Prsc.char ' ')
                        *> charInBlock
      Prsc.skipMany $ Prsc.char ' '
      title <- Prsc.optionMaybe $ textBetween '"' '"' charInBlock
      Prsc.char ')'
      return $ Link text href title

    -- FIXME: This is wrong. Actually parse an email, otherwise
    -- default to link.
    -- FIXME: Doesn't encode emails
    autoLink :: Parser Inline
    autoLink = Prsc.try email <|> link
        where
          email :: Parser Inline
          email = do
            Prsc.char '<'
            Email <$> Prsc.many1 (Prsc.notFollowedBy (Prsc.char '@') *> charInBlock)
                   <> Prsc.string "@"
                   <> Prsc.manyTill charInBlock (Prsc.char '>')

          link :: Parser Inline
          link = do
            link <- textBetween '<' '>' charInBlock
            return $ Link link link Nothing

    refLink :: Parser Inline
    refLink = do
      text <- textBetween '[' ']' charInBlock
      Prsc.optional $ Prsc.char ' '
      -- Reference is either implicit or explicit
      ref <- Prsc.try (Prsc.string "[]" *> return (map toLower text))
         <|> map toLower <$> textBetween '[' ']' charInBlock
      refs <- snd <$> Prsc.getState
      case Map.lookup ref refs of
        Just (link, title) -> return $ Link text link title
        Nothing -> Prsc.unexpected "Link reference not found."






-- Shares almost all code with parseLink
-- TODO: Handle reference-style images.
image :: Parser Inline
image = do
  Prsc.char '!'
  alt <- textBetween '[' ']' charInBlock
  Prsc.char '('
  path <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char ')' <|> Prsc.char ' ')
                    *> charInBlock
  Prsc.skipMany $ Prsc.char ' '
  title <- Prsc.optionMaybe $ textBetween '"' '"' charInBlock
  Prsc.char ')'
  return $ Image path alt title
  
  

-- FIXME: Spec has rules on spaces around opening/closing signs
emphasis :: Parser Inline
emphasis = do
  opening <- Prsc.char '*' <|> Prsc.char '_'
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char opening) *> inline
  Prsc.char opening
  return $ Emphasis content



strong :: Parser Inline
strong = do
  opening <- Prsc.string "**" <|> Prsc.string "__"
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.string opening) *> inline
  Prsc.string opening
  return $ Strong content



codeSpan :: Parser Inline
codeSpan = do
  opening <- Prsc.many1 (Prsc.char '`') <* Prsc.optional (Prsc.char ' ')
  let closing = (Prsc.optional (Prsc.char ' ')) *> Prsc.string opening
  content <- Prsc.many1 $ Prsc.notFollowedBy closing *> charInBlock
  closing
  return $ CodeSpan content

    
