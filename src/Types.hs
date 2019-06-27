module Types (Document, Block (..), Span (..)) where

-- | Represents a documents element structure.
-- TODO: Track metadata?
type Document = [Block]

-- | Represents common block elements 
-- for a markup language (such as HTML).
-- TODO: Add more block elements as needed
data Block 
    = Paragraph [Span]
    | Header Int [Span] 
    deriving (Show, Eq)

-- | Represents common inline elements 
-- for a markup language (such as HTML).
-- TODO: Add more span elements as needed
data Span 
    = Text String 
    | Space -- Perhaps it'd be a good idea to count amount of spaces?
    | Link { text :: String, href :: String, title :: String }
    deriving (Show, Eq)

