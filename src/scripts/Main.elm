import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import String exposing (toInt)
import Json.Decode exposing (..)
import Time exposing (Time, second)
import Random exposing (generate)

import Types exposing (..)
import GridOperations exposing (..)
import ResizeBox exposing (..)
import PatternChooser exposing (..)

main : Program Never
main =
  App.program
    { init = init 20 20
    , view = mainView
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
    { grid : Grid
    , playing : Bool
    , speed : Int
    , time : Float
    , resizeBoxModel : ResizeBox.Model
    , patternChooserModel : PatternChooser.Model
    , dragging : Bool
    , dragState : CellState
    }


init : Int -> Int -> (Model, Cmd Msg)
init width height =
  (
   { grid = createGrid width height
   , playing = False
   , speed = 5
   , time = 0.0
   , resizeBoxModel = ResizeBox.init
   , patternChooserModel = PatternChooser.init
   , dragging = False
   , dragState = Alive
   }
  , Cmd.none
  )

-- UPDATE

type Msg
    = NoOp
    | Tick Float
    | Start
    | Pause
    | TogglePlaying
    | IncreaseSpeed
    | DecreaseSpeed
    | ChangeSpeed Int
    | ToggleCell Int Int
    | SetCell Int Int CellState
    | SetEnteredCell Int Int
    | SetGrid Grid
    | RandomizeGrid
    | DraggingOff
    | DraggingOn Int Int
    | OpenPatternChooser
    | ResizeBoxMsg ResizeBox.Msg
    | PatternChooserMsg PatternChooser.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Tick newTime ->
      if model.playing then
        ( { model | grid = updateGrid model.grid }, Cmd.none )
      else
        ( model, Cmd.none )

    Start ->
      ( { model | playing = True }, Cmd.none )

    Pause ->
      ( { model | playing = False }, Cmd.none )

    TogglePlaying ->
      ( { model | playing = not model.playing }, Cmd.none )

    IncreaseSpeed ->
      ( { model | speed = model.speed + 1 }, Cmd.none )

    DecreaseSpeed ->
      ( { model | speed = model.speed - 1 }, Cmd.none )

    ChangeSpeed newSpeed ->
      ( { model | speed = newSpeed }, Cmd.none )

    SetCell row col cellState ->
      let
        newGrid =
          setCell model.grid { row = row, col = col } cellState in
      ( { model | grid = newGrid }, Cmd.none )

    SetEnteredCell row col ->
      let
        newGrid =
          setCell model.grid { row = row, col = col } model.dragState in
      case ( model.dragging ) of
        True -> ( { model | grid = newGrid }, Cmd.none )
        False -> ( model, Cmd.none )

    ToggleCell row col ->
      let
        newState = case (getCellState model.grid { row = row, col = col } ) of
                       Alive -> Dead
                       Dead -> Alive
        newGrid =
          setCell model.grid { row = row, col = col } newState in
      ( { model | grid = newGrid }, Cmd.none )

    SetGrid grid->
      ( { model | grid = grid }, Cmd.none )

    RandomizeGrid ->
      ( model, Random.generate SetGrid ( getRandomGrid ( getRowNum model.grid ) ( getColNum model.grid ) ) )

    DraggingOn row col ->
      let
        newState = case (getCellState model.grid { row = row, col = col } ) of
                     Alive -> Dead
                     Dead -> Alive in
      ( { model | dragging = True, dragState = newState }, Cmd.none )

    DraggingOff ->
      ( { model | dragging = False }, Cmd.none )

    OpenPatternChooser ->
      let
        pcModel = model.patternChooserModel
        updatedPCModel = { pcModel | visible = True }
      in
      ( { model | patternChooserModel = updatedPCModel }, Cmd.none )

    ResizeBoxMsg subMsg ->
       let
         (updatedResizeBoxModel, resizeBoxCmd) = ResizeBox.update subMsg model.resizeBoxModel
         newModel = { model | resizeBoxModel = updatedResizeBoxModel }
       in
         case subMsg of
           ResizeBox.Submit ->
             let newGrid = createGrid model.resizeBoxModel.rows model.resizeBoxModel.cols in
             ( { newModel | grid = newGrid  }, Cmd.map ResizeBoxMsg resizeBoxCmd)
           _ ->
             ( newModel, Cmd.map ResizeBoxMsg resizeBoxCmd)

    PatternChooserMsg subMsg ->
       let
         (updatedPCModel, cmd) = PatternChooser.update subMsg model.patternChooserModel
         newModel = { model | patternChooserModel = updatedPCModel }
         newGrid = case updatedPCModel.chosenPattern of
                     Nothing -> newModel.grid
                     Just pattern -> pattern.definition
       in
         case subMsg of
           PatternChooser.Submit ->
             ( { newModel | grid = newGrid }, Cmd.map PatternChooserMsg cmd)
           _ ->
             ( newModel, Cmd.map PatternChooserMsg cmd)


-- VIEW


cellView : Int -> Int -> CellState -> Html Msg
cellView rowNum colNum cellState =
  let
    mouseDownEvent = onWithOptions "mousedown" defaultOptions (Json.Decode.succeed ( DraggingOn rowNum colNum ) )
    mouseUpEvent = onWithOptions "mouseup" defaultOptions (Json.Decode.succeed ( DraggingOff ) )
    clickEvent = onWithOptions "click" defaultOptions (Json.Decode.succeed ( ToggleCell rowNum colNum ) )
    mouseEnterEvent = onWithOptions "mouseenter" defaultOptions (Json.Decode.succeed ( SetEnteredCell rowNum colNum ) ) in
  case cellState of
      Alive -> td [ mouseEnterEvent, clickEvent, mouseDownEvent, mouseUpEvent, class "alive" ] [ text "o" ]
      Dead ->  td [ mouseEnterEvent, clickEvent, mouseDownEvent, mouseUpEvent, class "dead" ] [ text "x" ]

rowView : Int -> (Array CellState) -> Html Msg
rowView rowNum row =
  tr [ ] ( Array.toList ( Array.indexedMap ( cellView rowNum ) row ) )

gridView : Array (Array CellState) -> Html Msg
gridView grid =
  table [ class "grid-table noselect" ] ( Array.toList ( Array.indexedMap rowView grid ) )

speedChanger : Model -> Html Msg
speedChanger model =
   Html.form [ class "form-inline navbar-form" ]
     [
      label [ for "speed-input" ] [ text "Speed:" ]
     , div [ class "input-group spinner" ]
       [
        input [
           id "speed-input"
          , type' "text"
          , class "form-control speed-input pull-right"
          , Html.Attributes.value (toString model.speed)
          , onInput (\speed -> toInt speed |> Result.toMaybe |> Maybe.withDefault 0 |> ChangeSpeed ) ] []
       , div [ class "input-group-btn-vertical"]
         [
          button [ class "btn btn-default", type' "button", onClick IncreaseSpeed ] [ span [] [ text "+" ] ]
         , button [ class "btn btn-default", type' "button", onClick DecreaseSpeed ] [ span [] [ text "-" ] ]
         ]
       ]
     ]

navBar : Model -> Html Msg
navBar model =
  let startBtnText = if model.playing then "Stop" else "Start"
      startBtnClass = if model.playing then "btn-danger" else "btn-success" in
  div [ class "container" ]
    [
     div [ class "navbar-header" ] [ a [ class "navbar-brand", href "#" ] [ text "Game of Life" ] ]
    , div [ class "collapse navbar-collapse" ]
    [
     button [ class ( "btn navbar-btn " ++ startBtnClass ), onClick TogglePlaying ]
       [
        text startBtnText
       ]
    , button [ class ( "btn navbar-btn open-pattern-chooser" ), onClick OpenPatternChooser ]
       [
        text "Patterns"
       ]
    , ul [ class "nav navbar-nav navbar-right" ]
      [
       App.map ResizeBoxMsg (ResizeBox.view model.resizeBoxModel )
      , li []
        [
         a [ href "#", onClick RandomizeGrid ]
           [
            text "Randomize"
           ]
        ]
      , li [] [ speedChanger model ]
      ]
    ]
  ]

mainView : Model -> Html Msg
mainView model =
  div [ class ( if model.patternChooserModel.visible then "modal-open" else "" ) ]
    [
     nav [ class "navbar navbar-default" ] [ navBar model ]
    , div [ class "container main-view" ] [ gridView model.grid ]
    , App.map PatternChooserMsg ( PatternChooser.view model.patternChooserModel )
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every ( 2 * second / toFloat model.speed ) Tick
