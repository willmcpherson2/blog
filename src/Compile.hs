module Compile (compile, compilePreview) where

import Data.Char (isSpace)
import Parse (Document(..), Element(..), Part(..), Style(..))

compile :: Document -> String
compile (Document eles) = concatMap compileEle eles

compilePreview :: Document -> String -> String
compilePreview (Document (ele : _)) path =
  "<a href='" ++ path ++ "'>" ++ compileEle ele ++ "</a>"

compileEle :: Element -> String
compileEle (Element style parts) = case style of
  Rule -> "<br><hr><br>"
  Title -> "<h1>" ++ compileParts parts ++ "</h1>"
  Heading -> "<h2>" ++ compileParts parts ++ "</h2>"
  Subheading -> "<h3>" ++ compileParts parts ++ "</h3>"
  CodeBlock ->
    "<div class='code-block'><code><pre>"
      ++ compileParts parts
      ++ "</pre></code></div>"
  Bold -> "<b>" ++ compileParts parts ++ "</b>"
  Italics -> "<i>" ++ compileParts parts ++ "</i>"
  CodeInline ->
    "<span class='code-inline'><code>" ++ compileParts parts ++ "</code></span>"
  Link ->
    let (caption, link) = splitAtLink parts
    in
      "<a href='" ++ compileParts link ++ "'>" ++ compileParts caption ++ "</a>"
  Paragraph -> "<p>" ++ compileParts parts ++ "</p>"

splitAtLink :: [Part] -> ([Part], [Part])
splitAtLink parts =
  let
    partIsSpace (CharPart char) = isSpace char
    (link, caption) = break partIsSpace $ reverse parts
  in (reverse $ tail caption, reverse link)

compileParts :: [Part] -> String
compileParts = concatMap compilePart

compilePart :: Part -> String
compilePart part = case part of
  CharPart char -> [char]
  ElementPart ele -> compileEle ele
