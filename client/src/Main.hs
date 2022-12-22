module Main where

import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Text (pack, unpack)
import JSDOM (currentWindow)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import Post (posts, Post (..))
import Reflex.Dom
import Tulip (getResult)

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: Widget x ()
headElement = do
  el "title" $ text "willmcpherson2"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/style.css") $ text "blog"

bodyElement :: Widget x ()
bodyElement = do
  header
  currentWindow >>= \case
    Just window -> getLocation window >>= getPathname >>= route
    Nothing -> error "no window"

header :: Widget x ()
header = elClass "div" "header" $ do
  el "h2" $ elAttr "a" ("href" =: "/") $ text "willmcpherson2"
  el "nav" $ do
    elAttr "a" ("href" =: "/posts") $ text "blog"
    text " "
    elAttr "a" ("href" =: "/tulip") $ text "tulip"
  el "hr" blank

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
previews = mapM_ preview $ sortOn date posts

preview :: Post -> Widget x ()
preview post = elClass "div" "preview" $ do
  elAttr "a" ("href" =: pack ("/posts/" <> key post)) $ text $ pack $ title post
  text " "
  el "date" $ text $ pack $ show $ date post

post :: String -> Widget x ()
post k = case find (\p -> key p == k) posts of
  Just p -> do
    el "h1" $ text $ pack $ title p
    el "date" $ text $ pack $ show $ date p
    content p
  Nothing -> notFound

tulip :: Widget x ()
tulip = do
  area <- textAreaElement $ def & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "code-input"
  let inputEvent = _textAreaElement_input area
  output <- foldDyn (\source _ -> pack $ getResult $ unpack source) "" inputEvent
  el "pre" $ elClass "code" "code-block" $ dynText output

notFound :: Widget x ()
notFound = text "page not found!"
