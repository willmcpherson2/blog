module Main (main) where

import Reflex.Dom
import Utils (header, notFound)

main :: IO ()
main = mainWidget $ do
  header
  notFound
