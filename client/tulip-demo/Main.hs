module Main (main) where

import Data.Text (Text, pack, unpack)
import Markup.M (m)
import Reflex.Dom
import Text.RawString.QQ
import Tulip (getResult)
import Utils (header)

main :: IO ()
main = mainWidget $ do
  header
  intro
  area <-
    textAreaElement $
      def
        & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "code-input" <> "rows" =: "5")
        & textAreaElementConfig_initialValue .~ tulipExample
  let inputEvent = _textAreaElement_input area
  let handle = pack . getResult . unpack
  output <- foldDyn (\source _ -> handle source) (handle tulipExample) inputEvent
  el "pre" $ elClass "code" "code-block" $ dynText output

intro :: Widget x ()
intro =
  [m|

#[
Tulip Interpreter
]

p[
@[[github.com/willmcpherson2/tulip][https://github.com/willmcpherson2/tulip]]
]

p[
A function `|[[x y]]| is a parameter and a body.
Multiple parameters `|[[x y z]]| are interpreted as `|[[x [y z]]]|.
]

p[
Applications `[(f x)] let you apply values to functions.
The result is the body of the function on the left with its parameter substituted by the value on the right.
Multiple applications `[(f x y)] are interpreted as `[((f x) y)].
]

p[
A variable `[x] refers to a previously defined value.
They can be shadowed.
]

p[
Holes `[_] can't be bound, and so can be used to ignore an argument.
They can also be used to signify undefined values.
]

p[
Definitions `[(x y)] can be recursive.
They have the same syntax as applications, but are top-level only.
]

p[
The entrypoint to a program is `[main].
]

|]

tulipExample :: Text
tulipExample =
  [r|(true [x y x])
(false [x y y])
(not [x (x false true)])
(main (not true))|]
