module Main where

import Data.List.Split (splitOn)
import JSDOM (currentWindow)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import MarkupDemo
import NotFound
import Post (post, previews)
import Reflex.Dom
import TulipDemo

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: Widget x ()
headElement = do
  el "title" $ text "willmcpherson2"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/style.css") blank

bodyElement :: Widget x ()
bodyElement = do
  header
  currentWindow >>= \case
    Just window -> getLocation window >>= getPathname >>= route
    Nothing -> error "no window"

header :: Widget x ()
header = elClass "div" "header" $ do
  elClass "h2" "button-home" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"
  el "nav" $ do
    elAttr "a" ("href" =: "/posts") $ text "posts"
    text " "
    elAttr "a" ("href" =: "/tulip") $ text "tulip"
    text " "
    elAttr "a" ("href" =: "/markup") $ text "markup"

route :: String -> Widget x ()
route pathname = case filter (not . null) $ splitOn "/" pathname of
  [] -> index
  ["posts"] -> previews
  ["posts", name] -> post name
  ["tulip"] -> tulip
  ["markup"] -> markup
  _ -> notFound

index :: Widget x ()
index = previews
