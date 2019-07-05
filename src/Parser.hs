-- FIXME: Don't expose all functions.
module Parser where

import Types
import Control.Applicative ((<|>))
import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)
import Data.Maybe

mdSymbols = ['*', '_', '[', ']', '(', ')', '#']

-- | Parse a string formatted with Markdown.
-- FIXME: Adding newlines might not be the best way.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.parse parseDocument "" (s ++ "\n\n")
             in case parsed of
                Right doc -> doc
                Left e -> error (show e) -- Shouldn't happen.



parseDocument :: Parser Document
parseDocument = Prsc.manyTill parseBlock documentEnding
  where
    documentEnding = do
      Prsc.skipMany $ Prsc.oneOf " \n"
      Prsc.eof



parseBlock :: Parser Block
parseBlock = Prsc.try parseHeader
         <|> parseParagraph



-- TODO: Handle Setext-style headers.
parseHeader :: Parser Block
parseHeader = do
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
  spans <- Prsc.many1 parseSpan
  paragraphEnding
  return $ Paragraph spans
    where
      -- FIXME: Refactor.
      paragraphEnding = do
        Prsc.skipMany (Prsc.char ' ')
        Prsc.endOfLine
        Prsc.many1 $ Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine



parseSpan :: Parser Span
parseSpan = Prsc.try parseNl
        <|> Prsc.try parseLineBreak
        <|> Prsc.try parseSpace
        <|> Prsc.try parseLink
        <|> Prsc.try parseEmph
        <|> parseText
        <|> parseSymbol


parseLineBreak :: Parser Span
parseLineBreak = do
  Prsc.count 2 $ Prsc.char ' '
  Prsc.endOfLine
  return LineBreak


parseNl :: Parser Span
parseNl = do
  Prsc.endOfLine
  Prsc.notFollowedBy (Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine)
  return Space



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


-- Parses any char but respects block endings.
-- As with parseNl it only cares for paragraphs atm.
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
  


-- FIXME: Spec has rules on spaces around opening/closing signs
parseEmph :: Parser Span
parseEmph = do
  opening <- Prsc.char '*' <|> Prsc.char '_'
  -- Look into if why Prsc.manyTill is not sufficient here:
  content <- Prsc.many1 $ Prsc.notFollowedBy (Prsc.char opening) *> parseSpan
  Prsc.char opening
  return $ Emph content






