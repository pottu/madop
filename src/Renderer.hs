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
   "<h" ++ show level ++ ">" ++ renderSpans spans ++ "</h" ++ show level ++ ">"

renderBlock (CodeBlock lines) = 
  "<pre><code>" ++ renderLines lines ++ "</code></pre>"
    where
      renderLines :: [String] -> String
      renderLines [] = ""
      renderLines (l:ls) = concatMap convert l ++ "\n" ++ renderLines ls 
      
      convert :: Char -> String
      convert '<' = "&lt;"
      convert '>' = "&gt;"
      convert '&' = "&amp;"
      convert  c  = [c]



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
renderSpan (Strong content) = "<strong>" ++ renderSpans content ++ "</strong>"
renderSpan LineBreak = "<br />"
renderSpan (Code content) = "<code>" ++ content ++ "</code>"

