module Header exposing (..)

import Html exposing (Html,div,text,span,a)
import Html.Attributes exposing (..)
import StyleSheet exposing (..)


header : List (Html div)
header =
  [
    div[headerStyle][
      span [][
        logoName
      ]
      ,span [][
         a [spanHeader ,href "#" ][text "Home"]
        ,a [spanHeader, href "#"][text "About US"]
        ,a [spanHeader,href "#"][text "Contact US"]
      ]
    ]
  ]

logoName :Html div
logoName = span [logoStyle][text ""]
