module Footer exposing (..)

import Html exposing (Html,div,text,span,a)
import Html.Attributes exposing (..)
import StyleSheet exposing (..)

footer : List (Html a)
footer =
  [
    div [footerStyle][
      text "Copyright 2016.  Made with love by Khaled Omar"
      ]
  ]
