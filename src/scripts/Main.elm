import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import TodoInput exposing (..)
import Time exposing (Time, millisecond)

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
  , currentEvent: CurrentEvent
  }


init : (Model, Cmd Msg)
init =
  (
   {
     todoInputModel = TodoInput.init
   , events = []
   , currentEvent = Latest
   }
  , Cmd.none
  )

createEvent : TodoInput.Msg -> Maybe Event
createEvent subMsg =
  case subMsg of
    TodoInput.Create todo -> Just
      { type' = Types.Create
      , payload = todo
      , progress = 0
      }
    TodoInput.Delete todo -> Just
      { type' = Types.Delete
      , payload = todo
      , progress = 0
      }
    Switch todo state -> Just
      { type' = Types.Update
      , payload = { todo | completed = state }
      , progress = 0
      }
    UpdateDescription todo text -> Just
      { type' = Types.Update
      , payload = { todo | description = text }
      , progress = 0
      }
    _ -> Nothing

condenseEvents : ( List Event ) -> ( List Todo )
condenseEvents events =
  let reducer event todoList =
    case event.type' of
      Types.Create -> event.payload :: todoList
      Types.Update -> List.map (\todo -> if todo.id == event.payload.id then event.payload else todo ) todoList
      Types.Delete -> List.filter (\todo -> todo.id /= event.payload.id ) todoList
  in
  List.foldl reducer [] events

-- UPDATE

type Msg
    = NoOp
    | TodoInputMsg TodoInput.Msg
    | SetCurrent Int
    | Tick Float

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    TodoInputMsg subMsg ->
       let
         (updatedTodos, todoInputCmd) = TodoInput.update subMsg model.todoInputModel
         newEvent =  createEvent subMsg
         newEventList = case newEvent of
                          Just event -> model.events ++ [ event ]
                          Nothing -> model.events
         newModel = { model | todoInputModel = updatedTodos, events = newEventList }
       in
         ( newModel, Cmd.map TodoInputMsg todoInputCmd)
    Tick newTime ->
      let
        eventMapper = (\ev -> { ev | progress = Basics.min ( ev.progress + 0.004 ) 1 } )
      in
      ( { model | events = List.map eventMapper model.events }, Cmd.none )
    SetCurrent idx ->
      let
         storedEvents = List.filter ( \ev -> ev.progress == 1 ) model.events
         current = if idx >= ( List.length storedEvents ) - 1 then Latest else Index idx
      in
      ( { model | currentEvent = current }, Cmd.none )


-- VIEW

navBar : Html Msg
navBar =
  div [ class "container" ]
    [
     div [ class "navbar-header" ] [ a [ class "navbar-brand", href "#" ] [ text "Event Sourcing" ] ]
    ]

completedMark : Bool -> Html Msg
completedMark isCompleted =
  if isCompleted
  then span [ class "complete glyphicon glyphicon-ok"] []
  else span [ class "incomplete glyphicon glyphicon-remove"] []


eventInTransitView : Event -> Html Msg
eventInTransitView event =
  let
    ( eventClass, eventName ) =
      case event.type' of
        Types.Create -> ( "success", "Create" )
        Types.Delete -> ( "danger", "Delete" )
        Types.Update -> ( "info", "Update" )
    quadEaseProgress = if event.progress <= 0.5
                  then ( ( event.progress * 2 ) ^ 4 ) / 2
                  else ( 1 - ( ( ( 1 - event.progress ) * 2 ) ^ 4 ) / 2 )
    distance = ( toString ( ( Basics.round ( 140 * quadEaseProgress ) - 40 ) ) ++ "%" )
  in
    h3 []
      [
       span [
          class ( "label label-lg label-" ++ eventClass ++ " event")
         , style [ ( "left", distance ) ]
         ]
         [
          text eventName
         ]
      ]

wireView : ( List Event ) -> ( List ( Html Msg ) )
wireView events =
  let
    isInTransit = (\ev -> ev.progress > 0 && ev.progress < 1)
  in
    ( div [ class "hr" ] [ div [ class "arrow-head-right" ] [] ] )
      ::
    ( List.map eventInTransitView ( List.filter isInTransit events ) )

eventStoredView : Int -> Int -> Event -> Html Msg
eventStoredView highlight idx event =
  let
    eventClass =
      case ( highlight == idx, event.type' ) of
        ( True, _ ) -> "active"
        ( _, Types.Create ) -> "success"
        ( _, Types.Delete ) -> "danger"
        ( _, Types.Update ) -> "info"
    eventName =
      case event.type' of
        Types.Create -> "Create"
        Types.Delete -> "Delete"
        Types.Update -> "Update"
  in
    tr
    [ class eventClass, onClick ( SetCurrent idx ) ]
    [
     td [] [ text ( toString idx ) ]
    , td [] [ text ( toString event.payload.id ) ]
    , td [] [ text eventName ]
    , td [] [ text event.payload.description ]
    , td [] [ completedMark event.payload.completed ]
    ]

storeView: CurrentEvent -> ( List Event ) -> ( List ( Html Msg ) )
storeView current events =
  let
    isStored = (\ev -> ev.progress == 1)
    storedEvents = List.filter isStored events
    headers =
      thead []
        [
         tr []
           [
            td [] [ text "Step" ]
           , td [] [ text "ID" ]
           , td [] [ text "Type" ]
           , td [] [ text "Summary" ]
           , td [] [ span [ class "glyphicon glyphicon-saved" ] [] ]
           ]
        ]
    highlightIdx =
      case current of
        Latest -> ( List.length storedEvents - 1 )
        Index n -> n
  in
    [
     table
       [ class "table table-condensed" ]
       [
        headers
       , tbody []
         ( List.indexedMap ( eventStoredView highlightIdx ) storedEvents )
       ]
    ]


materializedTodoView : Todo -> Html Msg
materializedTodoView todo =
  tr
    []
    [
     td [] [ text todo.description ]
    , td [] [ completedMark todo.completed ]
    ]


materializedView: ( List Todo ) -> ( List ( Html Msg ) )
materializedView todos =
  let
    headers =
      thead []
        [
         tr []
           [
            td [] [ text "Summary" ]
           , td [] [ span [ class "glyphicon glyphicon-saved" ] [] ]
           ]
        ]
  in
    [
     table
       [ class "table table-condensed" ]
       [
        headers
       , tbody []
         ( List.map materializedTodoView todos )
       ]
    ]

mainView : Model -> Html Msg
mainView model =
  let
    storedEvents = List.filter ( \ev -> ev.progress == 1 ) model.events
    eventsToCurrent =
      case model.currentEvent of
        Latest -> storedEvents
        Index n -> List.take ( n + 1 ) storedEvents
  in
  div [ ]
    [
     nav [ id "navbar", class "navbar navbar-default" ] [ navBar ]
    , div [ class "container main-view" ]
      [
       div [ class "row upper-row" ]
         [
          div [ class "todo-input well col-md-4" ] [ App.map TodoInputMsg ( TodoInput.view model.todoInputModel ) ]
         , div [ class "input-logger-wire wire col-md-4" ] ( wireView model.events )
         , div
            [ class "event-store well well-sm col-md-4" ] ( storeView model.currentEvent model.events )
         ]
      , div [ class "row middle-row" ]
        [
        div [ class "wire col-md-4 col-md-offset-8" ]
          [
           div [ class "vr" ] [ div [ class "arrow-head-down" ] [] ]
          ]
        ]
      , div [ class "row lower-row" ]
        [
         div [ class "output well col-md-4" ] []
        , div [ class "store-view-wire wire col-md-4" ] [
            div [ class "hr" ] [ div [ class "arrow-head-left" ] [] ]
           ]
        , div [ class "materialized-view well col-md-4" ] ( materializedView ( condenseEvents eventsToCurrent ) )
        ]
      ]
    ]
    

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick
