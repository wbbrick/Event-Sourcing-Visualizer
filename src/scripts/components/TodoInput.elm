module TodoInput exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onWithOptions, targetValue)
import Html.Attributes exposing (..)
-- import String exposing ( toInt )
import Json.Decode exposing (..)
import Types exposing (..)


type alias Model = ( List Todo )
init : Model
init = []

-- MESSAGES

type Msg
    = NoOp
    | Create Todo
    | Delete Todo
    | Switch Todo Bool
    | UpdateDescription Todo String

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
      NoOp ->
        model ! []
      Create todo ->
        ( todo :: model , Cmd.none )

      Delete todo ->
        ( List.filter (\t -> t == todo) model, Cmd.none )

      Switch todo state ->
        let
          mapper =
            (\t -> if t == todo then { t | completed = state } else t )
        in
        ( ( List.map mapper model ), Cmd.none )

      UpdateDescription todo text ->
        let
          mapper =
            (\t -> if t == todo then { t | description = text } else t )
        in
        ( ( List.map mapper model ), Cmd.none )


-- VIEW

todoView : Todo -> Html Msg
todoView todo =
  ul [ class "todo" ]
    [
     input [ type' "checkbox", class "completed", onCheck ( Switch todo ) ] []
     , input [ Html.Attributes.value todo.description, onInput ( UpdateDescription todo ) ] []
    ]

todoDecoder : Decoder Msg
todoDecoder =
  let
    todo =
      object2 Todo
        ("completed" := bool)
        ("description" := string)
  in
    Json.Decode.map Create todo

newTodoView : Html Msg
newTodoView =
  let
    options = { stopPropagation = True, preventDefault = True }
  in
  Html.form [ onWithOptions "input" options todoDecoder  ]
    [
     input [ type' "checkbox", class "completed" ] []
    , input [ Html.Attributes.value "" ] []
    ]


view : Model -> Html Msg
view model =
  div []
    [
     newTodoView,
     li [ class "todos" ]
       ( List.map todoView model )
    ]
