module Types exposing (..)
import Array exposing (Array)

type CellState = Alive | Dead

type alias Position =
  { row : Int
  , col : Int
  }

type alias Grid = Array ( Array CellState )
