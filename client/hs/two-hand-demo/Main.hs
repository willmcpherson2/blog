module Main (main) where

import Data.Text (Text, pack, unpack)
import Reflex.Dom
import TwoHand (report)

main :: IO ()
main = mainWidgetInElementById "two-hand-demo" $ do
  area <-
    textAreaElement $
      def
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "codeInput" <> "rows" =: "5")
        & textAreaElementConfig_initialValue .~ twoHandExample
  let inputEvent = _textAreaElement_input area
  let handle = pack . report . unpack
  output <- foldDyn (\source _ -> handle source) (handle twoHandExample) inputEvent
  el "pre" $ elClass "code" "sourceCode" $ dynText output

twoHandExample :: Text
twoHandExample = "{([Jam])}"
