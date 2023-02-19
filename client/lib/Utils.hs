module Utils (header, route, notFound) where

import Control.Arrow ((>>>))
import Data.List.Split (splitOn)
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import Reflex.Dom

header :: Widget x ()
header = elClass "div" "header" $ do
  elClass "h2" "button-home" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"
  el "nav" $ do
    elAttr "a" ("href" =: "/posts") $ text "posts"
    text " "
    elAttr "a" ("href" =: "/music") $ text "music"
    text " "
    elAttr "a" ("href" =: "/markup-demo") $ text "markup"
    text " "
    elAttr "a" ("href" =: "/two-hand-demo") $ text "two-hand"
    text " "
    elAttr "a" ("href" =: "/particle-life") $ text "particle-life"

notFound :: Widget x ()
notFound = text "page not found!"

route :: ([String] -> Widget x a) -> Widget x a
route f =
  currentWindowUnchecked
    >>= getLocation
    >>= getPathname
    >>= (splitOn "/" >>> filter (not . null) >>> f)
