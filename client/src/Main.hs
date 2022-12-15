module Main where

import Data.List (find, sort)
import Data.List.Split (splitOn)
import Data.Text (pack)
import Debug.Trace (traceShowId)
import JSDOM (currentWindow)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import Post (Post (..))
import Posts (posts)
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

header :: Widget x ()
header = do
  el "h1" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"

route :: String -> Widget x ()
route pathname = case filter (not . null) $ splitOn "/" pathname of
  [] -> index
  ["posts"] -> previews
  ["posts", name] -> post name
  ["tulip"] -> tulip
  _ -> notFound

index :: Widget x ()
index = previews

previews :: Widget x ()
previews = mapM_ preview $ sort posts

preview :: Post -> Widget x ()
preview post = el "div" $ elAttr "a" ("href" =: pack ("/posts/" <> key post)) $ text $ pack $ title post

post :: String -> Widget x ()
post k = case find (\p -> key p == k) posts of
  Just p -> do
    el "h2" $ text $ pack $ title p
    el "date" $ text $ pack $ show $ date p
    el "p" $ text $ pack $ content p
  Nothing -> notFound

tulip :: Widget x ()
tulip = error "not implemented"

notFound :: Widget x ()
notFound = error "not implemented"
