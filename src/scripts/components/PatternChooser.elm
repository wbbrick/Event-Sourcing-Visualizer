module PatternChooser exposing (..)

import Types exposing (..)
import Patterns exposing (patterns)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



type alias Model =
  {
    visible : Bool
  , chosenGrid : Maybe Grid
  }

init : Model
init =
  {
    visible = False
  , chosenGrid = Just patterns.blinker
  }

-- MESSAGES

type Msg
    = Show
    | Hide
    | Choose Grid
    | Submit

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
      Show ->
        ( { model | visible = True }, Cmd.none )

      Hide ->
        ( { model | visible = False }, Cmd.none )

      Choose grid ->
        ( { model | chosenGrid = Just grid }, Cmd.none )

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
                  text "Popular Game of Life Patterns"
                 ]
              ]
           , div [ class "modal-body" ]
             [
              text "body"
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
