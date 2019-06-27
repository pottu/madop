module Renderer (renderHTML) where

import Types

renderHTML :: Document -> String
renderHTML = concatMap ((++"\n") . renderBlock)

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderSpans content ++ "</p>"
renderBlock (Header level spans) = 
  -- FIXME: Ensure level <= 6!
  let content = renderSpans spans
   in "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"
-- Add cases when Block expands.

renderSpans :: [Span] -> String
renderSpans = concatMap renderSpan

renderSpan :: Span -> String
renderSpan (Text s) = s
renderSpan (Space) = " "
renderSpan (Link text href title) =
  "<a href=\"" ++ href ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
-- Add cases when Span expands.

