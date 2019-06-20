module Renderer (renderHTML) where

import Types

renderHTML :: Document -> String
renderHTML [] = ""
renderHTML (block:doc) = renderBlock block ++ renderHTML doc

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderSpans content ++ "</p>"

renderSpans :: [Span] -> String
renderSpans = concatMap renderSpan

renderSpan :: Span -> String
renderSpan (Text s) = s
-- Add cases when Span expands.

