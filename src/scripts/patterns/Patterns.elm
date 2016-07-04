module Patterns exposing (..)

import Types exposing (CellState, Grid)
import Array exposing (fromList)

-- shorthand for pattern definition
a : CellState
a = Types.Alive

d : CellState
d = Types.Dead

listToGrid: List ( List CellState ) -> Grid
listToGrid pattern =
  fromList ( List.map fromList pattern )

type alias PatternList =
  {
    blinker : Grid
  }

patterns : PatternList
patterns =
  {
    blinker =
      listToGrid
      [
       [d,d,d,d,d]
      ,[d,d,a,d,d]
      ,[d,d,a,d,d]
      ,[d,d,a,d,d]
      ,[d,d,d,d,d]
      ]
  }
