import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import String exposing (toInt)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Time exposing (Time, second)
import Random exposing (Generator, map)

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
    , resizeBoxCols : Int
    , resizeBoxRows : Int
    , resizeBoxVisible : Bool
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
   , resizeBoxCols = 20
   , resizeBoxRows = 20
   , resizeBoxVisible = False
   , dragging = False
   , dragState = Alive
   }
  , Cmd.none
  )

type CellState = Alive | Dead

type alias Position =
  { row : Int
  , col : Int
  }

type alias Grid = Array ( Array CellState )

createGrid : Int -> Int -> Grid
createGrid width height =
  Array.initialize height ( \_ -> Array.initialize width ( \_ -> Dead ) )

getCellState : Grid -> Position -> CellState
getCellState grid {row, col} =
  Maybe.withDefault Dead ( Array.get col ( Maybe.withDefault Array.empty ( Array.get row grid ) ) )

getColNum : Grid -> Int
getColNum grid = Array.length ( Maybe.withDefault Array.empty ( Array.get 0 grid ) )

getRowNum : Grid -> Int
getRowNum grid = Array.length grid

getNeighborPositions : Position -> List Position
getNeighborPositions {row, col} =
  let filter = List.filter (\posn -> posn.row /= row || posn.col /= col) in
  filter
  (
   List.foldl
     (\posn posns -> List.append
        ( (\x -> List.map (\y -> {row = x, col = y} ) [col-1 .. col+1] ) posn)
        posns
     )
     []
     [row-1 .. row+1]
  )


getNeighborValues : Grid -> Position -> List CellState
getNeighborValues grid position =
  List.map (getCellState grid) (getNeighborPositions position )

getTotalLivingNeighbors : Grid -> Position -> Int
getTotalLivingNeighbors grid position =
  List.length
    (List.filter (\cell -> cell == Alive) (getNeighborValues grid position))

setCell : Grid -> Position -> CellState -> Grid
setCell grid {row, col} cellState =
  let maybeRow = Array.get row grid in
  case maybeRow of
      Nothing -> grid
      Just rowArr -> Array.set row (Array.set col cellState rowArr) grid

getNewCellState : Grid -> Position -> CellState
getNewCellState grid position =
  let
    livingNeighbors =
      getTotalLivingNeighbors grid position
    currentState =
      getCellState grid position
  in
    -- maintain status quo for cells with 2 living neighbors
    if livingNeighbors == 2 then
      currentState
    -- bring cells with 3 living neighbors to life
    else if livingNeighbors == 3 then
      Alive
    -- kill under- and over-populated cells
    else
      Dead

getRandomCellState : Generator CellState
getRandomCellState =
    Random.map ( \b -> if b then Alive else Dead ) Random.bool

getRandomGrid : Int -> Int -> Generator Grid
getRandomGrid rows cols =
  let rowGenerator = Random.map Array.fromList ( Random.list cols getRandomCellState ) in
  Random.map Array.fromList ( Random.list rows rowGenerator )

updateCell : Grid -> Position -> Grid
updateCell grid position =
  setCell grid position (getNewCellState grid position)

toggleCell : Grid -> Position -> Grid
toggleCell grid position =
  let currentCellState = getCellState grid position in
  case currentCellState of
      Alive -> setCell grid position Dead
      Dead -> setCell grid position Alive

updateGrid : Grid -> Grid
updateGrid grid =
  Array.indexedMap
    (\rowNum row ->
       Array.indexedMap
       (\colNum cell ->
          getNewCellState grid {row = rowNum, col = colNum}
       )
       row
    )
    grid

randomizeGrid : Grid -> Grid
randomizeGrid grid =
  Array.map
    (\row ->
       Array.map
       (\cell ->
          Alive
       )
       row
    )
  grid

getStartButtonText : Bool -> String
getStartButtonText isPlaying =
  case isPlaying of
      True -> "Stop"
      False -> "Start"

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
    | NewGrid Int Int
    | SetGrid Grid
    | RandomizeGrid
    | DraggingOff
    | DraggingOn Int Int
    | ToggleResizeBox
    | UpdateResizeBoxRows Int
    | UpdateResizeBoxCols Int

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

    NewGrid rows cols->
      ( { model | grid = createGrid rows cols }, Cmd.none )

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

    ToggleResizeBox ->
      ( { model | resizeBoxVisible = not model.resizeBoxVisible }, Cmd.none )

    UpdateResizeBoxCols cols ->
      ( { model | resizeBoxCols = cols }, Cmd.none )

    UpdateResizeBoxRows rows ->
      ( { model | resizeBoxRows = rows }, Cmd.none )


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

resizeBoxView : Model -> Html Msg
resizeBoxView model =
  let
    visibility = if model.resizeBoxVisible then "fade-in" else "fade-out"
    inputHandler = (\msg num -> toInt num |> Result.toMaybe |> Maybe.withDefault 0 |> msg )
    rows = Html.Attributes.value ( toString model.resizeBoxRows )
    cols = Html.Attributes.value ( toString model.resizeBoxCols )
  in
  Html.form [ class ( "form-inline navbar-form size-box " ++ visibility ) ]
    [
     input [ class "reset row input", rows, onInput ( inputHandler UpdateResizeBoxRows ) ] []
    , text "x"
    , input [ class "reset col input", cols, onInput ( inputHandler UpdateResizeBoxCols ) ] []
    , button [ class "btn btn-tiny btn-success" ]
      [
       span [ class "glyphicon glypicon-ok", property "aria-hidden" ( Encode.string "true" ) ] []
      ]
    ]

speedChanger : Model -> Html Msg
speedChanger model =
   Html.form [ class "form-inline navbar-form" ]
     [
      label [ for "speed-input" ] [ text "Speed:" ]
     , div [ class "input-group" ]
       [
        span [ class "input-group-btn speed-buttons" ]
          [ button [ class "btn btn-default btn-decrease-speed", type' "button", onClick DecreaseSpeed ] [ text "-" ] ]
       , input [
           id "speed-input"
          , type' "text"
          , class "form-control speed-input"
          , Html.Attributes.value (toString model.speed)
          , onInput (\speed -> toInt speed |> Result.toMaybe |> Maybe.withDefault 0 |> ChangeSpeed ) ] []
       , span [ class "input-group-btn" ]
         [ button [ class "btn btn-default btn-increase-speed", type' "button", onClick IncreaseSpeed ] [ text "+" ] ]
       ]
     ]

navBar : Model -> Html Msg
navBar model =
  div [ class "container" ]
    [
     div [ class "navbar-header" ] [ a [ class "navbar-brand", href "#" ] [ text "Game of Life" ] ]
  , div [ class "collapse navbar-collapse" ]
    [
     button [ class "btn btn-primary navbar-btn", onClick TogglePlaying ]
         [ text ( getStartButtonText model.playing ) ]
    , ul  [ class "nav navbar-nav navbar-right" ]
      [
       li [] [ speedChanger model ]
      , li []
        [ a [ href "#", onClick RandomizeGrid ]
            [ text "Randomize" ]
        ]
      , li [ class "size-switcher" ]
        [
         ( resizeBoxView model )
         , a [ href "#", onClick ToggleResizeBox, class ( "size-link " ++ if model.resizeBoxVisible then "fade-out" else "fade-in" ) ]
           [ text "New Grid" ]

        ]
      ]
    ]
  ]

mainView : Model -> Html Msg
mainView model =
  div []
    [
     nav [ class "navbar navbar-default" ] [ navBar model ]
    , div [ class "container main-view" ] [ gridView model.grid ]
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every ( 2 * second / toFloat model.speed ) Tick
