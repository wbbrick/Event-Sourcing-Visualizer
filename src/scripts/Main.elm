import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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

type alias Row = Array CellState
type alias Grid = Array Array CellState
