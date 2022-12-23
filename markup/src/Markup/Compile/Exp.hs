module Markup.Compile.Exp (compile) where

import Data.Text (pack)
import Language.Haskell.TH.Syntax (Exp (..), Q)
import Markup.Parse
import Reflex.Dom (blank, el, elAttr, elClass, text, (=:))

compile :: String -> Q Exp
compile = generateTop . parse

generateTop :: [Tree] -> Q Exp
generateTop = \case
  [] -> [|blank|]
  [t] -> generate t
  t : ts -> [|$(generate t) >> $(generateTop ts)|]

generate :: Tree -> Q Exp
generate = \case
  Leaf s -> [|text $ pack s|]
  Branch style ts -> case style of
    Plain -> generateTop ts
    Heading -> [|el "h2" $(generateTop ts)|]
    Subheading -> [|el "h3" $(generateTop ts)|]
    Paragraph -> [|el "p" $(generateTop ts)|]
    CodeBlock -> [|el "pre" $ elClass "code" "code-block" $(generateTop ts)|]
    Code -> [|elClass "code" "code-inline" $(generateTop ts)|]
    Italics -> [|el "i" $(generateTop ts)|]
    Link -> case ts of
      [caption, Leaf link] -> [|elAttr "a" ("href" =: link) $(generate caption)|]
      _ -> undefined
