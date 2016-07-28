module Structure exposing (..)

import Html exposing (Html , div , text, button)
import Html.Events exposing (onClick)
import Html.App

-- Model
type alias Model = Bool

init : (Model , Cmd Msg)
init = (False,Cmd.none)


-- Msg
type Msg = Expand | Collapse

-- view
view : Model -> Html Msg
view  model =
  if model then
    div []
    [
       div [] [ button [ onClick Collapse ] [text "collapse"], text "hide me if u can"]
    ]

  else
    div []
    [
      button [ onClick Expand ] [text "expand"]
    ]

--Update
update : Msg -> Model ->(Model ,Cmd Msg)
update msg model = case msg of
  Expand  -> (True ,Cmd.none)
  Collapse -> (False , Cmd.none)

  -- subscriptions
subscriptions : Model -> Sub Msg
subscriptions  model= Sub.none


main : Program Never
main = Html.App.program
  {
   init= init
  ,update = update
  ,view = view
  , subscriptions= subscriptions
  }
