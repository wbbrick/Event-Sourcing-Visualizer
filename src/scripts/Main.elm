import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

main : Program Never
main =
  App.program
    { init = init 5 5
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type alias Model =
    { grid : Grid
    , playing : Bool
    , speed : Int
    }


init : Int -> Int -> (Model, Cmd Msg)
init width height =
  (
   { grid = createGrid width height
   , playing = False
   , speed = 5
   }
  , Cmd.none
  )

type CellState = Alive | Dead

type alias Position =
  { row : Int
  , col : Int
  }

type alias Grid = Array (Array CellState)

createGrid : Int -> Int -> Grid
createGrid width height =
  Array.initialize height (\_ -> Array.initialize width (\_ -> Dead ) )

getCellState : Grid -> Position -> CellState
getCellState grid {row, col} =
  case Maybe.andThen (Array.get col grid) (Array.get row) of
      Just val -> val
      Nothing -> Dead

getNeighborPositions : Position -> List Position
getNeighborPositions {row, col} =
  List.foldl
    (\posn posns -> List.append
       ( (\x -> List.map (\y -> {row = x, col = y} ) [col-1 .. col+1] ) posn)
       posns
    )
    []
    [row-1 .. row+1]

getNeighborValues : Grid -> Position -> List CellState
getNeighborValues grid position =
  List.map (getCellState grid) (getNeighborPositions position )

getTotalLivingNeighbors : Grid -> Position -> Int
getTotalLivingNeighbors grid position =
  List.length
    (List.filter (\cell -> cell == Alive) (getNeighborValues grid position))

setCell : Grid -> Position -> CellState -> Grid
setCell grid {row, col} cellState =
  let maybeRowArr = Array.get row grid in
  case maybeRowArr of
      Nothing -> createGrid 0 0
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
       Array.indexedMap (\colNum cell -> getNewCellState grid {row = rowNum, col = colNum} ) row
    ) grid

-- UPDATE

type Msg
    = NoOp
    | Tick
    | Start
    | Pause
    | IncreaseSpeed
    | DecreaseSpeed
    | ChangeSpeed Int
    | ToggleCell Int Int
    | SetCell Int Int CellState
    | ClearGrid

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Tick ->
      if model.playing then
        ( { model | grid = updateGrid model.grid }, Cmd.none )
      else
        ( model, Cmd.none )

    Start ->
      ( { model | playing = True }, Cmd.none )

    Pause ->
      ( { model | playing = False }, Cmd.none )

    IncreaseSpeed ->
      ( { model | speed = model.speed + 1 }, Cmd.none )

    DecreaseSpeed ->
      ( { model | speed = model.speed - 1 }, Cmd.none )

    ChangeSpeed newSpeed ->
      ( { model | speed = newSpeed }, Cmd.none )

    SetCell row col cellState ->
      let
        newGrid =
          setCell model.grid { row = row, col = col } cellState
      in
          ( { model | grid = newGrid }, Cmd.none )

    ToggleCell row col ->
      let
        newState = case (getCellState model.grid { row = row, col = col } ) of
                       Alive -> Dead
                       Dead -> Alive
        newGrid =
          setCell model.grid { row = row, col = col } newState
      in
          ( { model | grid = newGrid }, Cmd.none )

    ClearGrid ->
      let
        rows =
          Array.length model.grid

        cols =
          case Array.get 0 model.grid of
              Nothing -> 0
              Just row -> Array.length row

      in
          ( { model | grid = createGrid rows cols }, Cmd.none )


-- VIEW


cellView : Int -> Int -> CellState -> Html Msg
cellView rowNum colNum cellState =
  let clickEvent = onClick ( ToggleCell rowNum colNum ) in
  case cellState of
      Alive -> td [ clickEvent, class "alive" ] [ text "o" ]
      Dead ->  td [ clickEvent, class "dead" ] [ text "x" ]

rowView : Int -> (Array CellState) -> Html Msg
rowView rowNum row =
  tr [ ] ( Array.toList ( Array.indexedMap ( cellView rowNum ) row ) )

gridView : Array (Array CellState) -> Html Msg
gridView grid =
  table [ class "grid-table" ] ( Array.toList ( Array.indexedMap rowView grid ) )

view : Model -> Html Msg
view model =
  div [] [ gridView model.grid ]
    -- [ h2 [] [text model.topic]
    -- , button [ onClick MorePlease ] [ text "More Please!" ]
    -- , br [] []
    -- , img [src model.gifUrl] []
    -- ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

