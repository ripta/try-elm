import Html exposing (div, h1, text)
import Markdown

helloWorld = text "Hello World!"

marked = Markdown.toHtml """

This is a test of Markdown:

  * apples
  * bananas
  * cherries
  * dates

"""

main =
  div [] [
      h1 [] [text "Hello"]
    , helloWorld
    , marked
  ]
