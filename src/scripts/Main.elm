-- import Html exposing (..)
-- import Html.App as App
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
import Array exposing (..)

-- Model

-- The state of the program
type alias Model =
    { grid : Grid
    , playing : Bool
    , speed : Int
    }

type CellState = Alive | Dead

type alias Position =
  { row : Int
  , col : Int
  }

type alias Grid = Array (Array CellState)

createGrid : Int -> Int -> Grid
createGrid width height =
  Array.initialize height (\_ -> Array.initialize width (\_ -> Dead ) )

getCellValue : Grid -> Position -> CellState
getCellValue grid {row, col} =
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
  List.map (getCellValue grid) (getNeighborPositions position )

getTotalLivingNeighbors : Grid -> Position -> Int
getTotalLivingNeighbors grid position =
  List.length
    (List.filter (\cell -> cell == Alive) (getNeighborValues grid position))

setCell : Grid -> Position -> CellState -> Grid
setCell grid {row, col} cellState =
  let maybeRowArr = Array.get row grid in
  case maybeRowArr of
      Nothing -> createGrid 0 0
      Just rowArr ->
        let colVal = (Array.get col rowArr) in
        case colVal of
          Nothing -> createGrid 0 0
          Just col -> Array.set row (Array.set col cellState rowArr) grid

-- updateCell : Grid -> Position -> Grid
-- updateCell grid position =
--   let
--     livingNeighbors =
--       getTotalLivingNeighbors grid position
--     cellSetter =
--       setCell grid position
--   in
--     case livingNeighbors of
--         0 -> cellSetter Dead


-- updateGrid : Grid -> Grid
-- updateGrid grid =
--   Array.indexedMap
--     (\rowNum row ->
--        Array.indexedMap (\colNum cell -> setCell grid {row = rowNum, col = colNum} Dead ) row
--     ) grid

-- UPDATE

type Msg
    = NoOp
    | Tick
    | Start
    | Pause
    | IncreaseSpeed
    | DecreaseSpeed
    | ChangeSpeed Int
    | UpdateCell Int Int CellState
    | ClearGrid

-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--   case msg of
--     NoOp ->
--       model ! []

--     Tick ->
--       if model.playing then
--         { model
--           | grid = updateGrid model.grid
--         }
--       else model
