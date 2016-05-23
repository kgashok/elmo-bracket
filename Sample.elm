module Sample exposing (..) -- where


import Html exposing (Html, div, text)
import Html.App
import Mouse
import Keyboard

-- MODEL

type alias Model = Int

init : (Model, Cmd Msg)
init =
  (0 , Cmd.none)

-- MESSAGES

type Msg
  = MouseMsg Mouse.Position
  | KeyMsg Keyboard.KeyCode

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text (toString model) ]

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMsg position ->
      (model + 1, Cmd.none)
    KeyMsg code ->
      (model + 2, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks MouseMsg
    , Keyboard.presses KeyMsg
    ]

-- MAIN
main : Program Never 
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }