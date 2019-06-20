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
