module Renderer (renderHTML) where

import Types

renderHTML :: Document -> String
renderHTML = concatMap renderBlock 

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderSpans content ++ "</p>"
-- Add cases when Block expands.

renderSpans :: [Span] -> String
renderSpans = concatMap renderSpan

renderSpan :: Span -> String
renderSpan (Text s) = s
-- Add cases when Span expands.

