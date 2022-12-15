module Main where

import Data.Text (pack)
import JSDOM (currentWindow)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import Reflex.Dom

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: Widget x ()
headElement = do
  el "title" $ text "willmcpherson2"

bodyElement :: Widget x ()
bodyElement = do
  header
  currentWindow >>= \case
    Just window -> getLocation window >>= getPathname >>= route
    Nothing -> error "no window"

route :: String -> Widget x ()
route pathname = el "pre" $ text $ pack pathname

header :: Widget x ()
header = do
  el "h1" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"
