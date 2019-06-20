module Types (Document, Block (..), Span (..)) where

-- | Represents a documents element structure.
type Document = [Block]

-- | Represents common block elements 
-- for a markup language (such as HTML).
data Block 
    = Paragraph [Span]
    deriving(Show)

-- | Represents common inline elements 
-- for a markup language (such as HTML).
data Span 
    = Text String 
    deriving(Show)

