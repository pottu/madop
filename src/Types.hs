module Types (Document, Block (..), Span (..)) where

-- | Represents a documents element structure.
-- TODO: Track metadata?
type Document = [Block]

-- | Represents common block elements 
-- for a markup language (such as HTML).
-- TODO: Add more block elements as needed
data Block 
    = Paragraph [Span]
    | Header Int String
    deriving (Show)

-- | Represents common inline elements 
-- for a markup language (such as HTML).
-- TODO: Add more span elements as needed
data Span 
    = Text String 
    deriving (Show)

