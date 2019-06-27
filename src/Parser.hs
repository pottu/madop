module Parser (parseMd) where

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
                Left e -> error (show e)


parseDocument :: Parser Document
parseDocument = Prsc.manyTill parseBlock documentEnding

documentEnding :: Parser ()
documentEnding = do
  Prsc.skipMany $ Prsc.oneOf " \n"
  Prsc.eof


parseBlock :: Parser Block
parseBlock = 
    Prsc.try parseHeader
    <|> parseParagraph



-- FIXME: Assumes EOL (as in: won't handle EOF)
--        Maybe append EOL to input at start.
-- TODO: Handle Setext-style headers.
-- FIXME: Parse span elements.
parseHeader :: Parser Block
parseHeader = do
    level <- length <$> Prsc.many1 (Prsc.char '#')
    Prsc.space
    header <- Prsc.manyTill parseSpan (Prsc.try headerEnding)
    return $ Header level header 
        where 
            headerEnding = do
                Prsc.skipMany (Prsc.char ' ')
                Prsc.skipMany (Prsc.char '#')
                Prsc.skipMany (Prsc.char ' ')
                Prsc.endOfLine



-- FIXME: This is just a dummy.
parseParagraph :: Parser Block
parseParagraph = do
    spans <- Prsc.manyTill parseSpan (Prsc.try paragraphEnding)
    return $ Paragraph spans

-- FIXME: Refactor.
paragraphEnding :: Parser () 
paragraphEnding = do
  Prsc.skipMany (Prsc.char ' ')
  Prsc.endOfLine
  Prsc.many1 $ Prsc.skipMany (Prsc.char ' ') *> Prsc.endOfLine
  return ()



parseSpan :: Parser Span
parseSpan = Prsc.try parseSpace
        <|> Prsc.try parseLink
        <|> parseText



parseText :: Parser Span
parseText = do
  -- FIXME: Refactor - Prsc.many Prsc.noneOf perhaps?
  text <- Prsc.manyTill Prsc.anyChar (Prsc.lookAhead $ Prsc.oneOf " \n[")
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









