module Main (main) where

import Data.Text (Text, pack, unpack)
import Markup.M (m)
import Reflex.Dom
import Text.RawString.QQ
import TwoHand (report)
import Utils (header)

main :: IO ()
main = mainWidget $ do
  header
  intro
  area <-
    textAreaElement $
      def
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "code-input" <> "rows" =: "5")
        & textAreaElementConfig_initialValue .~ twoHandExample
  let inputEvent = _textAreaElement_input area
  let handle = pack . report . unpack
  output <- foldDyn (\source _ -> handle source) (handle twoHandExample) inputEvent
  el "pre" $ elClass "code" "code-block" $ dynText output

intro :: Widget x ()
intro =
  [m|

#[
The Two Hand Language
]

p[
@[[github.com/willmcpherson2/two-hand][https://github.com/willmcpherson2/two-hand]]
]

p[This is my entry for @[[langjam #4][https://github.com/langjam/jam0004]]]

|]

twoHandExample :: Text
twoHandExample =
  [r|{([Jam])}|]
