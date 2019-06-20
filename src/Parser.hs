module Parser
    ( Document
    , parseMd 
    ) where


import qualified Text.Parsec as Prsc
import Text.Parsec.String (Parser)

type Document = [Block]

data Block 
    = Paragraph [Span]
    deriving(Show)

data Span 
    = Text String 
    deriving(Show)


parseMd :: String -> Document
parseMd s = let parsed = Prsc.parse parseBlock "" s 
             in case parsed of
                    Right block -> block : []
                    Left e -> error (show e)



parseBlock :: Parser Block
parseBlock = do
    text <- Prsc.many Prsc.anyChar
    return $ Paragraph $ [Text text]
