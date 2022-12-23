module Markup where

import Combinators
import Control.Monad (guard)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (pack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q)
import Parser
import Reflex.Dom (blank, el, elAttr, elClass, text, (=:))
import Stream (Stream (takeToken))

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

data Escaper = BracketEscape (NonEmpty Char) | LineEscape

compile :: String -> Q Exp
compile = generateTop . parse document

document :: Parser String [Tree]
document = do
  ts <- star tree
  pure $ concatLeafs ts

tree :: Parser String (Maybe Tree)
tree = branch <<|>> leaf

branch :: Parser String (Maybe Tree)
branch = runMaybeT $ do
  st <- lift style
  lift escaper >>= \case
    Just (BracketEscape e) -> do
      _ <- MaybeT open
      s <- lift $ takeToken `uptoIncluding` closeRaw e
      pure $ Branch st [Leaf s]
    Just LineEscape -> do
      lines <- lift rawLines
      pure $ Branch st [Leaf $ unlines lines]
    Nothing -> do
      _ <- MaybeT open
      ts <- lift $ tree `uptoIncluding` close
      pure $ Branch st (concatLeafs ts)

leaf :: Parser String (Maybe Tree)
leaf = runMaybeT $ do
  t <- MaybeT takeToken
  pure $ Leaf [t]

closeRaw :: NonEmpty Char -> Parser String (Maybe ())
closeRaw e = runMaybeT $ do
  _ <- MaybeT close
  e' <- MaybeT bracketEscape
  guard $ e == e'

rawLines :: Parser String [String]
rawLines = do
  line <- takeToken `upto` matchM '\n'
  lineEscape >>= \case
    Just {} -> (line :) <$> rawLines
    Nothing -> pure [line]

concatLeafs :: [Tree] -> [Tree]
concatLeafs = foldr go []
  where
    go t ts =
      case (t, ts) of
        (Leaf l, Leaf r : ts) -> Leaf (l <> r) : ts
        _ -> t : ts

style :: Parser String Style
style =
  keyword "##" Subheading
    <<|>> keyword "#" Heading
    <<|>> keyword "p" Paragraph
    <<|>> keyword "``" CodeBlock
    <<|>> keyword "`" Code
    <<|>> keyword "*" Italics
    <<|>> keyword "@" Link
    |>> pure Plain

keyword :: String -> Style -> Parser String (Maybe Style)
keyword s x = runMaybeT $ do
  _ <- MaybeT $ try $ matchesM s
  pure x

escaper :: Parser String (Maybe Escaper)
escaper =
  runMaybeT (BracketEscape <$> MaybeT bracketEscape)
    <<|>> runMaybeT (LineEscape <$ MaybeT lineEscape)

bracketEscape :: Parser String (Maybe (NonEmpty Char))
bracketEscape = plus $ try $ matchM '|'

lineEscape :: Parser String (Maybe ())
lineEscape = runMaybeT $ do
  _ <- MaybeT $ try $ matchesM "\n|"
  pure ()

open :: Parser String (Maybe Char)
open = try $ matchM '['

close :: Parser String (Maybe Char)
close = try $ matchM ']'

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
    CodeBlock -> [|el "pre" $ elClass "code" "code-block" $(generateTop ts)|]
    Code -> [|elClass "code" "code-inline" $(generateTop ts)|]
    Italics -> [|el "i" $(generateTop ts)|]
    Link -> case ts of
      [caption, Leaf link] -> [|elAttr "a" ("href" =: link) $(generate caption)|]
      _ -> undefined
