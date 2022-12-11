module Main where

import Reflex.Dom
import MyLib (someFunc)

main :: IO ()
main = do
  someFunc
  mainWidget $ text "hey"
