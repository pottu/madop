-- FIXME: Don't expose all functions.
module Renderer where

import Types
import Data.Maybe
import Data.List (intercalate)

encode :: Char -> String
encode '<' = "&lt;"
encode '>' = "&gt;"
encode '&' = "&amp;"
encode  c  = [c]


renderHTML :: Document -> String
renderHTML = concatMap ((++"\n\n") . renderBlock)

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderSpans content ++ "</p>"

renderBlock (Header level spans) = 
  let lvl = if level > 6 then 6 else level
   in "<h" ++ show lvl ++ ">" ++ renderSpans spans ++ "</h" ++ show lvl ++ ">"

renderBlock (CodeBlock lines) = 
  "<pre><code>" ++ renderLines lines ++ "</code></pre>"
    where
      renderLines :: [String] -> String
      renderLines [] = ""
      renderLines (l:ls) = concatMap encode l ++ "\n" ++ renderLines ls 

renderBlock (HtmlBlock s) = s

renderBlock (BlockQuote blocks) =
  "<blockquote>\n" ++ renderHTML blocks ++ "</blockquote>"

renderBlock HorizontalRule = "<hr />"
      



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

renderSpan SoftBreak = "\n"

renderSpan (Code content) = "<code>" ++ concatMap encode content ++ "</code>"

renderSpan (Image path alt Nothing) = 
  "<img src=\"" ++ path ++ "\" alt=\"" ++ alt ++ "\" />" 
renderSpan (Image path alt (Just title)) =
  "<img src=\"" ++ path ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\" />" 


