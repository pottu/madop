-- FIXME: Don't expose all functions.
module Renderer where

import Types
import Data.Maybe



renderHTML :: Document -> String
renderHTML = concatMap ((++"\n") . renderBlock)

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderSpans content ++ "</p>"

renderBlock (Header level spans) = 
  -- FIXME: Ensure level <= 6!
  let content = renderSpans spans
   in "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"

renderBlock (CodeBlock lines) = 
  "<pre><code>\n" ++ concatMap (++"\n") lines ++ "</pre></code>"
-- Add cases when Block expands.

renderSpans :: [Span] -> String
renderSpans = concatMap renderSpan

renderSpan :: Span -> String
renderSpan (Text s) = s
renderSpan (Space) = " "
renderSpan (Link text href (Just title)) =
  "<a href=\"" ++ href ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
renderSpan (Link text href Nothing) =
  "<a href=\"" ++ href ++ "\">" ++ text ++ "</a>"
renderSpan (Emph content) = "<em>" ++ renderSpans content ++ "</em>"
renderSpan LineBreak = "<br />"
-- Add cases when Span expands.

