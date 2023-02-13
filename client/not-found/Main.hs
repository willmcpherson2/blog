module Main (main) where

import Reflex.Dom
import Utils (header)

main :: IO ()
main = mainWidget $ do
  header
  text "page not found!"
