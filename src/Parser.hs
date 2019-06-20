module Parser
    ( parseMd 
    ) where


type Document = [Block]

data Block 
    = Paragraph [Span]
    deriving(Show)

data Span 
    = Text String 
--    | Bold String
    deriving(Show)



parseMd :: String -> Document
parseMd s = [Paragraph $ [Text s]]
