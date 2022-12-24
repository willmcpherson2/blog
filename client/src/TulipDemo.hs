module TulipDemo where

import Data.Text (Text, pack, unpack)
import Reflex.Dom
import Text.RawString.QQ
import Tulip (getResult)

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
tulipExample =
  [r|(true [x y x])
(false [x y y])
(not [x (x false true)])
(main (not true))|]
