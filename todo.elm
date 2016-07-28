module Todo exposing (..)

import Html exposing (Html ,div,button,text,input)
import Html.Events exposing (onClick)
import Html.App

-- Model
type alias Model= {
   lst : List String
  ,txt : String
}

init : (Model , Cmd Msg )
init = ({lst=[],txt="None"},Cmd.none)

-- Msg
type Msg = Add

-- View
view : Model -> Html Msg
view model =
  div [][
    div [ ][ button [onClick Add][ text "add"], input [][]]
  ]

-- Update
update : Msg -> Model -> (Model ,Cmd Msg)
update msg model = case msg of
  Add -> ({ model | lst=["lll"]++["kkll"]} , Cmd.none)

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions  model= Sub.none

-- Main
main : Program Never
main = Html.App.program
  {
   init= init
  ,update = update
  ,view = view
  , subscriptions= subscriptions
  }
