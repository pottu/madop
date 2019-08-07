module Types (Document, Block (..), Inline (..)) where

import Data.Maybe

-- | Represents a documents element structure.
-- TODO: Track metadata?
type Document = [Block]

-- | Represents common block elements 
-- for a markup language (such as HTML).
-- TODO: Add more block elements as needed
data Block 
    = Paragraph [Inline]
    | Header Int [Inline] 
    | CodeBlock [String]
    | HtmlBlock String
    | BlockQuote [Block]
    | HorizontalRule
    deriving (Show, Eq)

-- | Represents common inline elements 
-- for a markup language (such as HTML).
-- TODO: Add more span elements as needed
data Inline
    = Text String 
    | Space -- Perhaps it'd be a good idea to count amount of spaces?
    | LineBreak
    | SoftBreak
    | Link { text :: String, href :: String, title :: Maybe String }
    | Email String
    | Image { path :: String, alt :: String, title :: Maybe String }
    | Emphasis [Inline]
    | Strong [Inline]
    | CodeSpan String
    deriving (Show, Eq)

