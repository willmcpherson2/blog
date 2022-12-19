module Markup where

import Data.Foldable (asum)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Lit (..), Stmt (..))
import Reflex.Dom (blank, el, elAttr, text, (=:))

m :: QuasiQuoter
m =
  QuasiQuoter
    { quoteExp = pure . compile,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

data Tree = Leaf String | Branch Style [Tree]

data Style
  = Plain
  | Heading
  | Subheading
  | Paragraph
  | Code
  | Italics
  | Link

compile :: String -> Exp
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
  s -> case parseStyle s of
    ([], _) -> ([], Nothing)
    (s@(ch : rest), style) -> case parseOpen s of
      Nothing -> (rest, Just $ Leaf [ch])
      Just (rest, escape) -> case escape of
        [] -> Just . Branch style <$> branch rest
        escape ->
          let (rest', s) = escapeBranch escape rest
           in (rest', Just $ Branch style [Leaf s])

parseStyle :: String -> (String, Style)
parseStyle s =
  fromMaybe (s, Plain) $
    asum $
      map
        (\(keyword, style) -> (,style) <$> stripPrefix keyword s)
        [ ("##", Subheading),
          ("#", Heading),
          ("p", Paragraph),
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

generateTop :: [Tree] -> Exp
generateTop = \case
  [] -> VarE 'blank
  ts -> DoE $ map (NoBindS . generate) ts

generate :: Tree -> Exp
generate = \case
  Leaf s -> AppE (VarE 'text) (AppE (VarE 'pack) (LitE $ StringL s))
  Branch style ts -> case style of
    Plain -> generateTop ts
    Heading -> elExp "h2" ts
    Subheading -> elExp "h3" ts
    Paragraph -> elExp "p" ts
    Code -> elExp "pre" ts
    Italics -> elExp "i" ts
    Link -> case ts of
      [caption, Leaf link] ->
        AppE
          ( AppE
              (AppE (VarE 'elAttr) (LitE $ StringL "a"))
              (UInfixE (LitE $ StringL "href") (VarE '(=:)) (LitE $ StringL link))
          )
          (generate caption)
      _ -> undefined

elExp :: String -> [Tree] -> Exp
elExp tag ts = AppE (AppE (VarE 'el) (LitE $ StringL tag)) (generateTop ts)
