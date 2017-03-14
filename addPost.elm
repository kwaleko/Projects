module AddPost exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick,onInput)
import Html.Attributes exposing (..)
import Html.App
import StyleSheet exposing (pageStyle)
import Regex exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import String exposing (..)

--Model
type alias Model =
  {
    title : String
   ,content :  String
   ,tags : List String
  }


type alias Command = List (String,Msg)

--init
init : (Model,Cmd Msg)
init = (
          Model "Title" "" []
          ,Cmd.none
       )

initCommand :  Command
initCommand
  = [("B",B),("I",I),("line",Line),("Title",Blockquote),("highligh",Highligh),("Code",Code)]

--msg
type Msg = NoOp | NewContentTitle String | NewContentBody String | Post
            | B | I | Line | Blockquote | Highligh | Code

--view
view : Model -> Html Msg
view model = div [ pageStyle
                  ,pageStyle']
                [
                  titleView
                  ,div [] (commandView initCommand)
                  ,bodyView model
                  ,displayBody model
                ]

-- command bar to design the textarea
commandView : Command -> List (Html Msg)
commandView
  command =
    List.map (\(t,c)-> a [href "#",commandStyle,onClick c][ text t]) command


titleView : Html Msg
titleView =
  div [][
          span [spanStyle][text "Title"]
         ,span [spanStyle][
              input [titleInput,type' "textarea" ,placeholder "Write a Title here"][ ]
                          ]
        ]

bodyView : Model -> Html Msg
bodyView model =
   div [][
          span [][
            textarea [questionInput,onInput NewContentBody,value model.content][ ]
          ]
         ]

displayBody : Model -> Html Msg
displayBody model =
  div [][
        runParser htmlParser (model.content )
          ,div [][
            span [][
              button [onClick Post,postButton][text "Post"]
            ]
          ]
        ]

--update
update : Msg -> Model -> (Model ,Cmd Msg)
update msg model =
  case msg of
    NewContentTitle newTitle ->
      (
        { model | title = newTitle }
        ,Cmd.none
      )
    NoOp -> (model,Cmd.none)
    NewContentBody newContent ->
      (
        Model "" newContent []
        ,Cmd.none
      )
    B ->
      (
        { model | content =model.content ++ "(!BOLD!) (!BOLD!)" }
        ,Cmd.none
      )
    I ->
      (
        { model | content =model.content ++ "(!ITALIC!) (!ITALIC!)" }
        ,Cmd.none
      )
    Blockquote ->
      (
        { model | content =model.content ++ "(!TITLE!) (!TITLE!)" }
        ,Cmd.none
      )
    Line ->
      (
        { model | content =model.content ++ "(!LINE!)(!LINE!)" }
        ,Cmd.none
      )
    Code ->
      (
        { model | content =model.content ++ "(!CODE!) (!CODE!)" }
        ,Cmd.none
      )
    Highligh ->
      (
        { model | content =model.content ++ "(!HIGHLIGH!) (!HIGHLIGH!)" }
        ,Cmd.none
      )
    Post ->
      (
        model, Cmd.none
      )
--subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.none

--main
main : Program Never
main = Html.App.program
  {
   init= init
  ,update = update
  ,view = view
  , subscriptions= subscriptions
  }

--regular expression
matchToString : Match -> String
matchToString match =
  case  match.number `rem` 2  of
    0 ->"]"
    _ -> "B []["
    --_ -> ""

--command to msg
commandToMsg : Command -> List Msg
commandToMsg command =
  List.map (\(a,b) ->b) command

type Style =  Bold| Unstyled | Italic |   Coded | Lined | Titled

styleParserBold : Bool ->Bool ->Bool-> Bool-> Bool-> Parser ( List (List Char , (Style,Style,Style,Style,Style)))
styleParserBold bolded  italic coded lined titled=
  let --(bold,italic ,code,line ,Titled)
    style = (if bolded then Bold else Unstyled ,if italic then Italic else Unstyled,if coded then Coded else Unstyled,if lined then Lined else Unstyled,if titled then Titled else Unstyled)
  in
    (end `andThen` always (  succeed ( [] )))
    <|> (string "(!BOLD!)" `andThen`   \_ -> styleParserBold  (not bolded)  italic  coded lined titled )
    <|> (string "(!ITALIC!)" `andThen`  \_ -> styleParserBold  bolded  (not italic ) coded lined titled)
    <|> (string "(!CODE!)" `andThen`   \_ -> styleParserBold  bolded   italic  ( not coded) lined titled)
    <|> (string "(!LINE!)" `andThen`   \_ -> styleParserBold  bolded   italic   coded (not lined) titled)
    <|> (string "(!TITLE!)" `andThen`  \_ -> styleParserBold  bolded   italic   coded  lined (not titled) )
    <|> ( anyChar `andThen` \c -> (styleParserBold bolded italic  coded lined  titled)`andThen` \cs -> succeed ((c :: [],style) :: cs) )

{-styleParserCode : Bool -> Parser ( List (List Char, Style))
styleParserCode coded =
  let
    style = if coded then Coded else Unstyled
  in
  (end `andThen` always (succeed ([])))
  <|> (anyChar `andThen` \c ->  styleParserCode coded `andThen` \cs -> succeed ((c :: cs  ,style) ) )
-}
--styleCodeDecorator : Parser ( List (List Char, Style)) -> Parser ( List (List Char , (Style,Style,Style,Style,Style)))
--styleCodeDecorator parser =
--  \
--styleParserItalic : Bool -> Parser ( List (List Char , Style))
--styleParserItalic bolded =
--  let
--    style = if bolded then Italic else Unstyled
--  in
--    (end `andThen` always (  succeed ( [] )))
--    <|> (string "(!ITALIC!)" `andThen`  \_ -> styleParserItalic ( not bolded ) )
--    <|> ( anyChar `andThen` \c -> styleParserItalic bolded `andThen` \cs -> succeed ((c :: [],style) :: cs) )

--styleParser = styleParserItalic `andThen` \c -> styleParserBold

foldStyleHtml : List ( List Char , ( Style,Style,Style,Style,Style) ) -> List (Html Msg)
foldStyleHtml lst =List.map styleToHtml lst


styleToHtml : ( List Char, (Style ,Style,Style,Style,Style)) -> Html Msg
styleToHtml (a,b) =
  case b of
    (Bold,Italic,_,_,_) -> strong [] [u [][ text  (String.fromList a)]]
    (Unstyled,Unstyled,Unstyled,Unstyled,Unstyled) -> text (String.fromList a)
    (Unstyled,Italic,_,_,_) -> em [] [text (String.fromList a)]
    (Bold,Unstyled,_,_,_) ->strong [][ text  (String.fromList a)]
    (_,_,Coded,_,_) -> code [codeStyle][text (String.fromList a)]
    (_,_,_,Lined,_) -> br [][text "t"]
    (_,_,_,_,Titled) -> span [titleStyle][text (String.fromList a)]
    (_,_,_,_,_) -> text ""

htmlParser : Parser (List (Html Msg))
htmlParser =styleParserBold False False  False False False `andThen` (succeed << foldStyleHtml )

runParser : Parser (List (Html Msg)) -> String -> Html Msg
runParser parser str =
  case parse parser  str of
    (Ok htmls,_)-> div [] htmls
    (Err err, _) -> div [ style [("color", "red")] ] [ text <| toString <| err]

-- style
pageStyle' =
  style [
     ("padding","10px")
    ,("font-size","24px")
    ,("text-align","center")
    ,("font-family","Geneva")
      ]

spanStyle =
  style [("margin","10px")]

titleInput =
   style [
    ("width","90%")
    ,("height","25px")
    ,("font-family","Geneva")
    ,("font-size","20px")
         ]

questionInput =
  style [
     ("width","90%")
    ,("height","200px")
    ,("font-family","Geneva")
    ,("font-size","18px")
    ,("margin","20px")
        ]

commandStyle =
  style [
     ("margin","2px")
    ,("padding","4px")
    ,("background-color","#dcecef")
    ,("text-decoration","none")
    ,("color","#0c0f0e")
    ,("font-size","15px")
      ]

codeStyle =
  style [
    ("background-color","#becad8")
  ]

markStyle =
  style [
    ("background-color","yellow")
  ]

postButton =
  style [
     ("height","50px")
    ,("width","80px")
    ,("font-size","15px")
  ]

titleStyle =
  style [
    ( "letter-spacing","5px")
    ,("text-decoration","bold")
    ,("font-size","30px")
  ]
