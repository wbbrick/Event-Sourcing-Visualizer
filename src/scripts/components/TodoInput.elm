module TodoInput exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onBlur, targetValue, on, keyCode)
import Json.Decode as Json
import Html.Attributes exposing (..)
import Types exposing (..)

type alias Model =
  { todos: ( List Todo )
  , newTodo: Todo
  , nextId : Int
  }

init : Model
init =
  { todos = []
  , newTodo = emptyTodo 1
  , nextId = 2
  }

-- MESSAGES

type Msg
    = NoOp
    | Create Todo
    | Delete Todo
    | Switch Todo Bool
    | UpdateDescription Todo String
    | UpdateNewDescription String

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
      NoOp ->
        model ! []

      Create todo ->
        (
         { todos =  todo :: model.todos
         , newTodo = emptyTodo model.nextId
         , nextId = model.nextId + 1
         }
        , Cmd.none
        )

      Delete todo ->
        (
         { todos = List.filter (\t -> t /= todo) model.todos
         , newTodo = model.newTodo
         , nextId = model.nextId
         }
         , Cmd.none
        )

      Switch todo state ->
        let
          mapper =
            (\t -> if t == todo then { t | completed = state } else t )
        in
        (
         { todos = ( List.map mapper model.todos )
         , newTodo = model.newTodo
         , nextId = model.nextId
         }
         , Cmd.none
        )

      UpdateDescription todo text ->
        let
          mapper =
            (\t -> if t == todo then { t | description = text } else t )
        in
        (
         { todos =  ( List.map mapper model.todos )
         , newTodo = model.newTodo
         , nextId = model.nextId
         }
        , Cmd.none
        )

      UpdateNewDescription text ->
        let
          newTodo = model.newTodo
        in
          (
           { todos = model.todos
           , newTodo = { newTodo | description = text }
           , nextId = model.nextId
           }
          , Cmd.none
          )


-- VIEW

todoView : Todo -> Html Msg
todoView todo =
  li [ class "todo form" ]
    [
     div [ class "input-group" ]
       [
        div [ class "input-group-addon" ]
          [
           input [ type' "checkbox", onCheck ( Switch todo ) ] []
          ]
       , input [ type' "text", Html.Attributes.value todo.description, class "form-control", onInput ( UpdateDescription todo ) ] []
       , div [ class "input-group-btn" ]
         [
          button [ type' "button", class "form-control btn btn-danger", onClick ( Delete todo ) ]
            [
             span [ class "glyphicon glyphicon-remove" ] []
            ]
         ]
       ]
    ]

newTodoView : Todo -> Html Msg
newTodoView todo =
  let
    createIfExists todo = if todo.description /= "" then ( Create todo ) else NoOp
    tagger keyCode = if keyCode == 13 then ( createIfExists todo ) else NoOp
  in
    div
    [ class "input-group input-group-lg" ]
    [
     input
       [
        type' "text"
       , class "form-control"
       , placeholder "New Todo..."
       , Html.Attributes.value todo.description
       , onInput ( UpdateNewDescription )
       , onBlur ( createIfExists todo )
       , on "keyup" ( Json.map tagger keyCode )
       ] []
    ]

view : Model -> Html Msg
view model =
  div []
    [
     newTodoView model.newTodo,
     ul [ class "todos" ]
       ( List.map todoView model.todos )
    ]
