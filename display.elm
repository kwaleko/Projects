module Display exposing (..)
import Html exposing (Html,div,button,text,span,h1,p,a)
import Html.App
import Html.Attributes exposing (..)
import StyleSheet exposing (..)
import Header exposing (..)
import Http
import Task
import Json.Decode as Json exposing (..)


--MODEL
type alias Post
  ={
     title : String
    ,content : String
    ,tags : List String
    ,date : String
    ,author : String
  }

type alias Model
  = List Post

--init
init : (Model,Cmd Msg)
init = ([],getData)


--Msg
type Msg= NoOp | FetchSucceed Model | FetchFail Http.Error


-- VIEW
view : Model -> Html Msg
view model = div[] (
                    header
                    ++ displayPosts model
                    --++ footer
                    )


displayPosts : List Post -> List (Html div)
displayPosts model =List.map (\s->displayPost s) model


-- take a post and wrap it into an HTML element
displayPost : Post -> Html div
displayPost post =
            div [pageStyle][
               h1 [titleStyle][text post.title]
              ,div []
                (getTags post.tags)
                ,div [marignStyle] [
                  text "Posted on : "
                  ,span  [dateStyle][
                    text (post.date )
                    ]
                    ,span [marignStyle][text "By"]
                    ,span [authorStyle] [text post.author]
                ]
              ,p [paragraphStyle][text post.content]
            ]
--get a list of tags and cast it to list of HTML tag
getTags : List String ->List (Html a)
getTags lst =List.map (\s -> a[tagStyle,href "#"][text s]) lst



--UPDATE
update : Msg -> Model -> (Model , Cmd Msg)
update msg model = case msg of
  NoOp -> (model ,getData)
  FetchSucceed  posts -> (posts,Cmd.none)
  FetchFail errorMessage -> (errorFetch errorMessage model,Cmd.none)

errorFetch :Http.Error-> Model -> Model
errorFetch message model=
  List.map (\post -> Post (toString message) "" [] "" "") model


getData : Cmd Msg
getData =  Task.perform FetchFail FetchSucceed(Http.get listOfPostDecoder "http://localhost:8081/posts")


postDecoder : Json.Decoder Post
postDecoder = Json.object5
               Post
               ("title" := string)
               ("content" := string)
               ("tags" :=Json.list string)
               ("date" :=string)
               ("author" :=string)

listOfPostDecoder : Json.Decoder (List Post)
listOfPostDecoder = Json.list postDecoder
-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--Main
main : Program Never
main = Html.App.program
  {
   init= init
  ,update = update
  ,view = view
  , subscriptions= subscriptions
  }
