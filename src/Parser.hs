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
    header <- Prsc.manyTill Prsc.anyChar (Prsc.try headerEnding)
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
    text <- Prsc.manyTill Prsc.anyChar (Prsc.oneOf "\n")
    return $ Paragraph [Text text]
