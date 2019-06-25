module Parser (parseMd) where

import Types
import Control.Applicative ((<|>))
import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)


-- | Parse a string formatted with Markdown.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.parse parseBlock "" s 
             in case parsed of
                  Right block -> [block]
                  Left e -> error (show e)


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
    spans <- parseSpans
    return $ Paragraph spans



parseSpans :: Parser [Span]
parseSpans = Prsc.manyTill parseSpan Prsc.newline



parseSpan :: Parser Span
parseSpan = 
    Prsc.try parseLink
    <|> parseText



parseText :: Parser Span
parseText = do
    text <- Prsc.manyTill Prsc.anyChar (Prsc.lookAhead $ Prsc.oneOf "\n[")
    return $ Text text



-- FIXME: Forces link with title.
-- TODO: Handle reference-style links.
parseLink :: Parser Span 
parseLink = do
    text <- Prsc.char '[' *> Prsc.manyTill Prsc.anyChar (Prsc.char ']') 
    href <- Prsc.char '(' *> Prsc.manyTill Prsc.anyChar (Prsc.char ' ')
    title <- Prsc.manyTill Prsc.anyChar (Prsc.char ')')
    return $ Link text href title









