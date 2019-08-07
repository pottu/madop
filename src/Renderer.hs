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
renderHTML [] = ""
renderHTML (x:[]) = renderBlock x ++ "\n"
renderHTML (x:xs) = (renderBlock x ++ "\n\n") ++ renderHTML xs

renderBlock :: Block -> String
renderBlock (Paragraph content) = "<p>" ++ renderInlines content ++ "</p>"

renderBlock (Header level inlines) = 
  let lvl = if level > 6 then 6 else level
   in "<h" ++ show lvl ++ ">" ++ renderInlines inlines ++ "</h" ++ show lvl ++ ">"

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
      



renderInlines :: [Inline] -> String
renderInlines = concatMap renderInline

renderInline :: Inline -> String
-- map encode s?
renderInline (Text s) = s

renderInline (Space) = " "

renderInline (Link text href (Just title)) =
  "<a href=\"" ++ href ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"

renderInline (Link text href Nothing) =
  "<a href=\"" ++ href ++ "\">" ++ text ++ "</a>"

renderInline (Email mail) =
  "<a href=\"mailto:" ++ mail ++ "\">" ++ mail ++ "</a>"

renderInline (Emphasis content) = "<em>" ++ renderInlines content ++ "</em>"

renderInline (Strong content) = "<strong>" ++ renderInlines content ++ "</strong>"

renderInline LineBreak = "<br />"

renderInline SoftBreak = "\n"

renderInline (CodeSpan code) = "<code>" ++ concatMap encode code ++ "</code>"

renderInline (Image path alt Nothing) = 
  "<img src=\"" ++ path ++ "\" alt=\"" ++ alt ++ "\" />" 
renderInline (Image path alt (Just title)) =
  "<img src=\"" ++ path ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\" />" 


