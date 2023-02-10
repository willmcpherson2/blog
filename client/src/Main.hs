module Main where

import Data.List.Split (splitOn)
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import MarkupDemo
import NotFound
import Post (post, previews)
import Reflex.Dom
import TulipDemo
import Music
import ParticleLife

main :: IO ()
main = mainWidget bodyElement

bodyElement :: Widget x ()
bodyElement = do
  header
  currentWindowUnchecked >>= getLocation >>= getPathname >>= route

header :: Widget x ()
header = elClass "div" "header" $ do
  elClass "h2" "button-home" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"
  el "nav" $ do
    elAttr "a" ("href" =: "/posts") $ text "posts"
    text " "
    elAttr "a" ("href" =: "/tulip") $ text "tulip"
    text " "
    elAttr "a" ("href" =: "/markup") $ text "markup"
    text " "
    elAttr "a" ("href" =: "/music") $ text "music"
    text " "
    elAttr "a" ("href" =: "/particlelife") $ text "particlelife"

route :: String -> Widget x ()
route pathname = case filter (not . null) $ splitOn "/" pathname of
  [] -> index
  ["posts"] -> previews
  ["posts", name] -> post name
  ["tulip"] -> tulip
  ["markup"] -> markup
  ["music"] -> music
  ["particlelife"] -> canvas
  _ -> notFound

index :: Widget x ()
index = previews
