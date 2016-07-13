import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

main : Program Never
main =
  App.program
    { init = init
    , view = mainView
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type Status = Incomplete | Complete

type EventType = Update | Delete | Create

type alias Todo =
  {
    status: Status
  , description: String
  }

type alias Event =
  {
    type': EventType
  , payload: Todo
  , progress: Int
  }

type alias Model =
  {
    todos: ( List Todo )
  , events: ( List Event )
  }


init : (Model, Cmd Msg)
init =
  (
   {
     todos = []
   , events = []
   }
  , Cmd.none
  )

-- UPDATE

type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

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
          div [ class "todo-input col-md-4" ] []
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
