module Chess.App exposing (..)

import Chess.Game exposing (Game(..))
import Html exposing (Html, div, text, table, tr, td)
import Html.Attributes exposing (class)
import Browser

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model = { game: Game }

init = { game = GameStart  }

update msg model = init

view: Model -> Html msg
view model =
    table [] [
        tr [] [
            td [] [],
            td [ class "columnLabel" ] [text "a"],
            td [ class "columnLabel" ] [text "b"],
            td [ class "columnLabel" ] [text "c"],
            td [ class "columnLabel" ] [text "d"],
            td [ class "columnLabel" ] [text "e"],
            td [ class "columnLabel" ] [text "f"],
            td [ class "columnLabel" ] [text "g"],
            td [ class "columnLabel" ] [text "h"],
            td [] []
        ]
    ]

boardSize = 450

fieldSize : Float
fieldSize = boardSize / 9
