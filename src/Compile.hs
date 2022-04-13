module Compile (compile, compilePreview) where

import Data.List.NonEmpty (toList)
import GHC.Base
import Parse

compile :: Document -> String
compile Document{date, title, blocks} =
  compileDate date
    <> compileTitle title
    <> compileBlocks blocks

compilePreview :: String -> Title -> String
compilePreview path title =
  "<a href='" <> path <> "'>" <> compileTitle title <> "</a>"

compileDate :: Date -> String
compileDate = show

compileTitle :: Title -> String
compileTitle = \case
  Title s -> "<h1>" <> toList s <> "</h1>"
  TitleError e -> compileError e

compileBlocks :: [Block] -> String
compileBlocks = concatMap compileBlock

compileBlock :: Block -> String
compileBlock = \case
  Heading s -> "<h2>" <> toList s <> "</h2>"
  Subheading s -> "<h3>" <> toList s <> "</h3>"
  Code s ->
    "<div class='code-block'><code><pre>"
      <> toList s
      <> "</pre></code></div>"
  Paragraph eles -> "<p>" <> compileEles eles <> "</p>"
  BlockError e -> compileError e

compileEles :: NonEmpty Element -> String
compileEles = concatMap compileEle

compileEle :: Element -> String
compileEle = \case
  Bold eles -> "<b>" <> compileEles eles <> "</b>"
  Italics eles -> "<i>" <> compileEles eles <> "</i>"
  Link caption link ->
    "<a href='" <> toList link <> "'>" <> compileEles caption <> "</a>"
  InlineCode s ->
    "<span class='code-inline'><code>" <> toList s <> "</code></span>"
  Plain s -> toList s
  ElementError e -> compileError e

compileError :: Error -> String
compileError e = "<span class='error'><code>" <> show e <> "</code></span>"
