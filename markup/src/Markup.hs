module Markup where

import Data.Foldable (asum)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q)
import Reflex.Dom (blank, el, elAttr, text, (=:))

m :: QuasiQuoter
m =
  QuasiQuoter
    { quoteExp = compile,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

data Tree = Leaf String | Branch Style [Tree]

data Style
  = Plain
  | Subheading
  | Heading
  | Paragraph
  | CodeBlock
  | Code
  | Italics
  | Link

compile :: String -> Q Exp
compile = generateTop . parseTop

parseTop :: String -> [Tree]
parseTop s = case parse s of
  (rest, Just b@Branch {}) -> case rest of
    [] -> [b]
    rest -> b : parseTop rest
  (rest, _) -> case rest of
    [] -> []
    rest -> parseTop rest

parse :: String -> (String, Maybe Tree)
parse = \case
  [] -> ([], Nothing)
  ']' : rest -> (rest, Nothing)
  ch : rest ->
    let (rest', style) = parseStyle (ch : rest)
     in case parseOpen rest' of
          Nothing -> (rest, Just $ Leaf [ch])
          Just (rest'', []) -> Just . Branch style <$> branch rest''
          Just (rest'', escape) ->
            Just . Branch style . (: []) . Leaf <$> escapeBranch escape rest''

parseStyle :: String -> (String, Style)
parseStyle s =
  fromMaybe (s, Plain) $
    asum $
      map
        (\(keyword, style) -> (,style) <$> stripPrefix keyword s)
        [ ("##", Subheading),
          ("#", Heading),
          ("p", Paragraph),
          ("``", CodeBlock),
          ("`", Code),
          ("*", Italics),
          ("@", Link)
        ]

parseOpen :: String -> Maybe (String, String)
parseOpen s = case span (== '|') s of
  (escape, '[' : rest) -> Just (rest, escape)
  _ -> Nothing

escapeBranch :: String -> String -> (String, String)
escapeBranch escape = \case
  [] -> ([], [])
  ch : rest -> case ch of
    ']' -> case stripPrefix escape rest of
      Just rest -> (rest, [])
      Nothing -> (ch :) <$> escapeBranch escape rest
    ch -> (ch :) <$> escapeBranch escape rest

branch :: String -> (String, [Tree])
branch = fmap concatLeafs . go
  where
    go s = case parse s of
      (rest, Nothing) -> (rest, [])
      (rest, Just t) -> (t :) <$> go rest

concatLeafs :: [Tree] -> [Tree]
concatLeafs = \case
  Leaf l : Leaf r : ts -> concatLeafs $ Leaf (l <> r) : ts
  t : ts -> t : concatLeafs ts
  ts -> ts

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
    Heading -> [|el "h1" $(generateTop ts)|]
    Subheading -> [|el "h2" $(generateTop ts)|]
    Paragraph -> [|el "p" $(generateTop ts)|]
    CodeBlock -> [|el "pre" $ el "code" $(generateTop ts)|]
    Code -> [|el "code" $(generateTop ts)|]
    Italics -> [|el "i" $(generateTop ts)|]
    Link -> case ts of
      [caption, Leaf link] -> [|elAttr "a" ("href" =: link) $(generate caption)|]
      _ -> undefined
