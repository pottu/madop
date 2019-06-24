module Parser (parseMd) where

import Types
import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)


-- | Parse a string formatted with Markdown.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.parse parseBlock "" s 
             in case parsed of
                  Right block -> [block]
                  Left e -> error (show e)


-- FIXME: This is just a dummy.
parseBlock :: Parser Block
parseBlock = do
    text <- Prsc.many Prsc.anyChar
    return $ Paragraph $ [Text text]



-- FIXME: Assumes EOL (as in: won't handle EOF)
--        Maybe append EOL to input at start.
-- TODO: Handle Setext-style headers.
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
