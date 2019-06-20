module Parser
    ( Document
    , parseMd 
    ) where


import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)

-- TODO: Move types to separate file.
-- | Represents a documents element structure.
type Document = [Block]

data Block 
    = Paragraph [Span]
    deriving(Show)

data Span 
    = Text String 
    deriving(Show)


-- | Parse a string formatted with Markdown.
parseMd :: String -> Document
parseMd s = let parsed = Prsc.parse parseBlock "" s 
             in case parsed of
                    Right block -> block : []
                    Left e -> error (show e)



parseBlock :: Parser Block
parseBlock = do
    text <- Prsc.many Prsc.anyChar
    return $ Paragraph $ [Text text]
