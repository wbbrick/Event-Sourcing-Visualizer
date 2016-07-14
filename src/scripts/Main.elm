import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Types exposing (..)
import TodoInput exposing (..)

main : Program Never
main =
  App.program
    { init = init
    , view = mainView
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  {
    todoInputModel: TodoInput.Model
  , events: ( List Event )
  }


init : (Model, Cmd Msg)
init =
  (
   {
     todoInputModel = TodoInput.init
   , events = []
   }
  , Cmd.none
  )

-- UPDATE

type Msg
    = NoOp
    | TodoInputMsg TodoInput.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    TodoInputMsg subMsg ->
       let
         (updatedTodos, todoInputCmd) = TodoInput.update subMsg model.todoInputModel
         newModel = { model | todoInputModel = updatedTodos }
       in
         ( newModel, Cmd.map TodoInputMsg todoInputCmd)


-- VIEW

navBar : Html Msg
navBar =
  div [ class "container" ]
    [
     div [ class "navbar-header" ] [ a [ class "navbar-brand", href "#" ] [ text "Event Sourcing" ] ]
    ]

mainView : Model -> Html Msg
mainView model =
  div [ ]
    [
     nav [ class "navbar navbar-default" ] [ navBar ]
    , div [ class "container main-view" ]
      [
       div [ class "row upper-row" ]
         [
          div [ class "todo-input col-md-4" ] [ App.map TodoInputMsg ( TodoInput.view model.todoInputModel ) ]
         , div [ class "input-logger-wire wire col-md-4" ] []
         , div [ class "logger col-md-4" ] []
         ]
         , div [ class "row lower-row" ]
         [
          div [ class "event-store col-md-5" ] []
         , div [ class "store-view-wire wire col-md-2" ] []
         , div [ class "materialized-view col-md-5" ] []
         ]
      ]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
