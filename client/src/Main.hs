module Main where

import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)
import JSDOM (currentWindow)
import JSDOM.Custom.Window (getLocation)
import JSDOM.Generated.Location (getPathname)
import Post (posts, Post (..))
import Reflex.Dom
import Tulip (getResult)
import Markup.Compile.Widget (compile)
import Text.RawString.QQ

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
  area <-
    textAreaElement $
      def
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "code-input"
        & textAreaElementConfig_initialValue .~ tulipExample
  let inputEvent = _textAreaElement_input area
  let handle = pack . getResult . unpack
  output <- foldDyn (\source _ -> handle source) (handle tulipExample) inputEvent
  el "pre" $ elClass "code" "code-block" $ dynText output

tulipExample :: Text
tulipExample = [r|(true [x y x])
(false [x y y])
(not [x (x false true)])
(main (not true))|]

markup :: Widget x ()
markup = do
  area <-
    textAreaElement $
      def
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ "class" =: "code-input"
        & textAreaElementConfig_initialValue .~ markupExample
  let inputEvent = _textAreaElement_input area
  let handle = compile . unpack
  output <- foldDyn (\source _ -> handle source) (handle markupExample) inputEvent
  elClass "div" "box" $ dyn_ output

markupExample :: Text
markupExample = [r|Text has no style by default.
Styles are applied using brackets.
Here are the available styles:

#[Heading]

##[Subheading]

p[Paragraph]

*[italics]

@[[link]https://example.com]

`[inline code]

``[code block]

[You can also use brackets without a style.
This is useful for escaping or passing data to the link style.]

p[Because this is HTML, you need paragraphs to get newlines.]

p|[
To escape, add pipes around your brackets.
Then you can use brackets literally: []
]|

p|||[
Use multiple brackets if necessary: ||[]||
]|||

``
|You can also escape by starting lines with a pipe:
|
|``
||This is useful for pasting code.|]

notFound :: Widget x ()
notFound = text "page not found!"
