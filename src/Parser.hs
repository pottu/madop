-- FIXME: Don't expose all functions.
module Parser where

import Types
import Control.Applicative ((<|>))
import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)


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
      Prsc.endOfLine



parseParagraph :: Parser Block
parseParagraph = do
  spans <- Prsc.manyTill parseSpan (Prsc.try paragraphEnding)
  return $ Paragraph spans
  where
    -- FIXME: Refactor.
    paragraphEnding = do
      Prsc.skipMany (Prsc.char ' ')
      Prsc.endOfLine
      Prsc.many1 $ Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine



parseSpan :: Parser Span
parseSpan = Prsc.try parseSpace
        <|> Prsc.try parseLink
        <|> Prsc.try parseEmph
        <|> parseText



parseText :: Parser Span
parseText = do
  -- FIXME: Put reserved chars somewhere more appropiate.
  text <- Prsc.many $ Prsc.noneOf " \n[*_"
  return $ Text text



parseSpace :: Parser Span
parseSpace = do
  Prsc.space
  return Space



-- FIXME: Forces link with title.
-- TODO: Handle reference-style links.
parseLink :: Parser Span 
parseLink = do
    text <- Prsc.char '[' *> Prsc.manyTill Prsc.anyChar (Prsc.char ']') 
    href <- Prsc.char '(' *> Prsc.manyTill Prsc.anyChar (Prsc.char ' ')
    title <- Prsc.manyTill Prsc.anyChar (Prsc.char ')')
    return $ Link text href title



-- FIXME: Fail parsing if end of block.
parseEmph :: Parser Span
parseEmph = do
  opening <- Prsc.char '*' <|> Prsc.char '_'
  content <- Prsc.manyTill parseSpan (Prsc.char opening)
  return $ Emph content






