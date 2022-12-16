module Markup where

import Data.Text (pack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Lit (..), Stmt (..))
import Reflex.Dom

m :: QuasiQuoter
m =
  QuasiQuoter
    { quoteExp = pure . compile,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

data Tree = Leaf String | Branch Style [Tree]

data Style = Heading | Subheading | Paragraph | Code | Italics

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
  'h' : '1' : '[' : s -> branch Heading s
  'h' : '2' : '[' : s -> branch Subheading s
  'p' : '[' : s -> branch Paragraph s
  'c' : '[' : s -> branch Code s
  'i' : '[' : s -> branch Italics s
  ']' : s -> (Nothing, s)
  ch : s -> (Just $ Leaf [ch], s)
  [] -> (Nothing, [])

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
  Branch style ts -> AppE (AppE (VarE 'el) (LitE $ StringL $ styleToStr style)) (generateTop ts)

styleToStr :: Style -> String
styleToStr = \case
  Heading -> "h1"
  Subheading -> "h2"
  Paragraph -> "p"
  Code -> "pre"
  Italics -> "i"
