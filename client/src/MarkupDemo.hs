module MarkupDemo where

import Data.Text (Text, unpack)
import Reflex.Dom
import Markup.Compile.Widget (compile)
import Text.RawString.QQ

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