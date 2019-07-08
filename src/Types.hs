module Types (Document, Block (..), Span (..)) where

import Data.Maybe

-- | Represents a documents element structure.
-- TODO: Track metadata?
type Document = [Block]

-- | Represents common block elements 
-- for a markup language (such as HTML).
-- TODO: Add more block elements as needed
data Block 
    = Paragraph [Span]
    | Header Int [Span] 
    | CodeBlock [String]
    deriving (Show, Eq)

-- | Represents common inline elements 
-- for a markup language (such as HTML).
-- TODO: Add more span elements as needed
data Span 
    = Text String 
    | Space -- Perhaps it'd be a good idea to count amount of spaces?
    | LineBreak
    | Link { text :: String, href :: String, title :: Maybe String }
    | Emph [Span]
    deriving (Show, Eq)

