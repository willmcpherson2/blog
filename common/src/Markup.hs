module Markup where

import Data.Foldable (asum)
import Data.List (stripPrefix)
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
  (Just b@Branch {}, s) -> case s of
    [] -> [b]
    s -> b : parseTop s
  (_, s) -> case s of
    [] -> []
    s -> parseTop s

parse :: String -> (Maybe Tree, String)
parse = \case
  ']' : s -> (Nothing, s)
  ch : s -> case strToStyle (ch : s) of
    Just (style, s) -> branch style s
    Nothing -> (Just $ Leaf [ch], s)
  [] -> (Nothing, [])

strToStyle :: String -> Maybe (Style, String)
strToStyle s =
  asum $
    map
      (\(keyword, style) -> (style,) <$> stripPrefix keyword s)
      [ ("[", Plain),
        ("h1[", Heading),
        ("h2[", Subheading),
        ("p[", Paragraph),
        ("c[", Code),
        ("i[", Italics),
        ("l[", Link)
      ]

branch :: Style -> String -> (Maybe Tree, String)
branch style s =
  let (ts, s') = go s
   in (Just $ Branch style (concatLeafs ts), s')
  where
    go :: String -> ([Tree], String)
    go s = case parse s of
      (Nothing, s) -> ([], s)
      (Just t, s) ->
        let (ts, s') = go s
         in (t : ts, s')

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
      [caption, Branch _ [Leaf link]] ->
        AppE
          ( AppE
              (AppE (VarE 'elAttr) (LitE $ StringL "a"))
              (UInfixE (LitE $ StringL "href") (VarE '(=:)) (LitE $ StringL link))
          )
          (generate caption)
      _ -> undefined

elExp :: String -> [Tree] -> Exp
elExp tag ts = AppE (AppE (VarE 'el) (LitE $ StringL tag)) (generateTop ts)
