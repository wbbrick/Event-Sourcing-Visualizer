module PatternChooser exposing (..)

import Patterns exposing (Pattern, patterns)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (map)
import Types exposing (Grid, CellState)
import GridOperations exposing (getCellState)

type alias Model =
  {
    visible : Bool
  , chosenPattern : Maybe Pattern
  }

init : Model
init =
  {
    visible = False
  , chosenPattern = List.head patterns
  }

-- MESSAGES

type Msg
    = Show
    | Hide
    | Choose ( Maybe Pattern )
    | Submit

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
      Show ->
        ( { model | visible = True }, Cmd.none )

      Hide ->
        ( { model | visible = False }, Cmd.none )

      Choose pattern ->
        ( { model | chosenPattern = pattern }, Cmd.none )

      Submit ->
        ( { model | visible = False }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    visibility = if model.visible then "in" else "out"
  in
    div [ class ( "modal pattern-modal fade " ++ visibility ) ]
      [
      div [ class "modal-dialog" ]
        [
         div [ class "modal-content" ]
           [
            div [ class "modal-header" ]
              [
               span [ class "glyphicon glyphicon-remove pull-right", onClick Hide ] []
               , h4 [ class "pull-left" ]
                 [
                  text "Patterns"
                 ]
              ]
           , div [ class "modal-body" ]
             [
              ul [ class "pattern-list" ]
                ( List.map patternView patterns )
             ]
           , div [ class "modal-footer" ]
             [
              button [ class "btn btn-default", onClick Hide ]
                [
                 text "Cancel"
                ]
              , button [ class "btn btn-primary", onClick Submit ]
                [
                 text "View"
                ]
             ]
           ]
        ]
      ]

thumbnailGenerator : Grid -> List ( Html Msg )
thumbnailGenerator grid =
  let
    cellClass row col =
      case getCellState grid { row = row, col = col } of
        Types.Alive -> "alive"
        Types.Dead -> "dead"
  in
  Array.toList
    (
     Array.indexedMap
       (
        \rowNum row ->
          div [ class "thumbnail-row" ]
          (
           Array.toList
             (
              Array.indexedMap
                (
                 \colNum col ->
                   span [ class ( "thumbnail-cell " ++ ( cellClass rowNum colNum ) )  ] []
                )
                row
             )
          )
       )
       grid
    )

patternView : Pattern -> Html Msg
patternView pattern =
  li [ class "well pattern" ]
    [
     div [ class "pattern-thumbnail" ]
       ( thumbnailGenerator pattern.definition )
    , span [ class "pattern-name" ]
      [
       text pattern.name
      ]
    ]
