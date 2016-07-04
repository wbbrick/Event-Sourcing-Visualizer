module GridOperations exposing (..)

import Array exposing (..)
import Types exposing (..)
import Random exposing (Generator, map)

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
