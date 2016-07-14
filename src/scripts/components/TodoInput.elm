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
        let
          newTodoList =
            if todo.description == ""
            then model.todos
            else todo :: model.todos
        in
        (
         { todos = newTodoList
         , newTodo = emptyTodo model.nextId
         , nextId = model.nextId + 1
         }
        , Cmd.none
        )

      Delete todo ->
        (
         { todos = List.filter (\t -> t == todo) model.todos
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
  ul [ class "todo" ]
    [
     input [ type' "checkbox", class "completed", onCheck ( Switch todo ) ] []
     , input [ Html.Attributes.value todo.description, onInput ( UpdateDescription todo ) ] []
    ]

newTodoView : Todo -> Html Msg
newTodoView todo =
  let
    tagger keyCode = if keyCode == 13 then ( Create todo ) else NoOp
  in
  input
  [
   Html.Attributes.value todo.description
  , onInput ( UpdateNewDescription )
  , onBlur ( Create todo )
  , on "keyup" ( Json.map tagger keyCode )] []


view : Model -> Html Msg
view model =
  div []
    [
     newTodoView model.newTodo,
     li [ class "todos" ]
       ( List.map todoView model.todos )
    ]
