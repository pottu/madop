-- FIXME: Don't expose all functions.
module Parser where

import Types
import Control.Applicative ((<|>))
import qualified Text.Parsec as Prsc
import Data.Maybe

data ParserState = InParagraph 
                 | InHeader
                 | InCodeBlock
                 deriving (Eq, Show)
type Parser = Prsc.Parsec String ParserState

mdSymbols = ['*', '_', '[', ']', '(', ')', '#', '`', '!']



-- | Parse a string formatted with Markdown.
-- FIXME: Adding newlines might not be the best way.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.runParser parseDocument InParagraph "" (s ++ "\n\n")
             in case parsed of
                Right doc -> doc
                Left e -> error (show e) -- Shouldn't happen.



parseDocument :: Parser Document
parseDocument = Prsc.manyTill parseBlock (Prsc.try documentEnding)
  where
    documentEnding = do
      Prsc.skipMany $ Prsc.oneOf " \n"
      Prsc.eof



parseBlock :: Parser Block
parseBlock = Prsc.try parseHeader
         <|> Prsc.try parseCodeBlock
         <|> parseParagraph



-- TODO: Handle Setext-style headers.
parseHeader :: Parser Block
parseHeader = do
  Prsc.putState InHeader
  level <- length <$> Prsc.many1 (Prsc.char '#')
  Prsc.many1 Prsc.space
  header <- Prsc.manyTill parseSpan (Prsc.try headerEnding)
  return $ Header level header 
  where 
    headerEnding = do
      Prsc.skipMany (Prsc.char ' ')
      Prsc.skipMany (Prsc.char '#')
      Prsc.skipMany (Prsc.char ' ')
      Prsc.skipMany1 Prsc.endOfLine



parseParagraph :: Parser Block
parseParagraph = do
  Prsc.putState InParagraph 
  spans <- Prsc.many1 parseSpan
  paragraphEnding
  return $ Paragraph spans
    where
      -- FIXME: Refactor.
      paragraphEnding = do
        Prsc.skipMany (Prsc.char ' ')
        Prsc.endOfLine
        Prsc.many1 $ Prsc.try (Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine)



parseCodeBlock :: Parser Block
parseCodeBlock = do
  Prsc.putState InCodeBlock
  content <- Prsc.many1 codeLine
  Prsc.skipMany $ Prsc.try (Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine)
  return $ CodeBlock content
    where
      codeLine = (Prsc.count 4 (Prsc.char ' ') <|> Prsc.string "\t")
              *> Prsc.manyTill Prsc.anyChar Prsc.endOfLine



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
  state <- Prsc.getState
  case state of
    InParagraph -> do
      Prsc.endOfLine
      Prsc.notFollowedBy (Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine)
      return Space
    _ -> Prsc.unexpected "Rule only applies in paragraph"



parseSymbol :: Parser Span
parseSymbol = do
  c <- Prsc.oneOf mdSymbols
  return $ Text [c]



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
parseLink = do
  text <- parseTextBetween '[' ']' parseChar
  Prsc.char '('
  href <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char ')' <|> Prsc.char ' ')
                    *> parseChar
  Prsc.skipMany $ Prsc.char ' '
  title <- Prsc.optionMaybe $ parseTextBetween '"' '"' parseChar
  Prsc.char ')'
  return $ Link text href title



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
   in do
        content <- Prsc.many1 $ Prsc.notFollowedBy closing *> parseChar
        closing
        return $ Code content

