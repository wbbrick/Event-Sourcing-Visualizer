module ResizeBox exposing (..)

import Html exposing (..)
import Html.Events exposing ( onClick, onInput )
import Html.Attributes exposing (..)
import String exposing ( toInt )
import Json.Encode as Encode


type alias Model =
  {
    rows : Int
  , cols : Int
  , visible : Bool
  }

init : Model
init =
  {
    rows = 20
  , cols = 20
  , visible = False
  }

-- MESSAGES

type Msg
    = Show
    | Hide
    | UpdateCols Int
    | UpdateRows Int
    | Submit

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
      Show ->
        ( { model | visible = True }, Cmd.none )

      Hide ->
        ( { model | visible = False }, Cmd.none )

      UpdateCols newCols ->
        ( { model | cols = newCols }, Cmd.none )

      UpdateRows newRows ->
        ( { model | rows = newRows }, Cmd.none )

      Submit ->
        ( { model | visible = False }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    visibility = if model.visible then "fade-in" else "fade-out"
    inputHandler = (\msg num -> toInt num |> Result.toMaybe |> Maybe.withDefault 0 |> msg )
    rows = Html.Attributes.value ( toString model.rows )
    cols = Html.Attributes.value ( toString model.cols )
    cancel = onClick Hide
    submit = onClick Submit
  in
    li [ class "size-switcher" ]
       [
        Html.form [ class ( "form-inline navbar-form size-box " ++ visibility ) ]
          [
           input [ class "reset row input", rows, onInput ( inputHandler UpdateRows ) ] []
          , text "x"
          , input [ class "reset col input", cols, onInput ( inputHandler UpdateCols ) ] []
          ,  span [ submit, class "glyphicon glyphicon-ok submit", property "aria-hidden" ( Encode.string "true" ) ] []
          ,  span [ cancel, class "glyphicon glyphicon-remove cancel", property "aria-hidden" ( Encode.string "true" ) ] []
          ]
       , a [ href "#", onClick Show, class ( "size-link " ++ if model.visible then "fade-out" else "fade-in" ) ]
         [ text "New Grid" ]
       ]
