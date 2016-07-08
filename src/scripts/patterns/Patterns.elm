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

type alias Pattern =
  {
    name : String,
    definition : Grid
  }

patterns : List Pattern
patterns =
  [
   {
     name = "Blinker"
   , definition =
      listToGrid
      [
       [d,d,d,d,d]
      ,[d,d,a,d,d]
      ,[d,d,a,d,d]
      ,[d,d,a,d,d]
      ,[d,d,d,d,d]
      ]
   }
  , {
     name = "Boat"
   , definition =
      listToGrid
      [
       [d,d,d,d,d]
      ,[d,a,a,d,d]
      ,[d,a,d,a,d]
      ,[d,d,a,d,d]
      ,[d,d,d,d,d]
      ]
   }
  , {
     name = "Toad"
   , definition =
      listToGrid
      [
       [d,d,d,d,d,d]
      ,[d,d,d,d,d,d]
      ,[d,d,a,a,a,d]
      ,[d,a,a,a,d,d]
      ,[d,d,d,d,d,d]
      ,[d,d,d,d,d,d]
      ]
   }
  ]
