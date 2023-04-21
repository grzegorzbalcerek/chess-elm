module Chess.GameTest exposing (..)

import Expect exposing (Expectation)
import List exposing (map)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Figure exposing (FigureType(..), Figure, figure)
import Chess.Field exposing (field, showField)
import Chess.Game exposing (..)
import Chess.FigureMoves exposing (..)

suite : Test
suite =
    describe "GameMoves" [
        test "isFieldEmpty false" <| \_ ->
            isFieldEmpty GameStart (field 2 2) |> Expect.equal False,
        test "isFieldEmpty grue" <| \_ ->
            isFieldEmpty GameStart (field 3 3) |> Expect.equal True,
        test "freeDestinations 1" <| \_ ->
            (map showField <| freeDestinations GameStart <| figureMoves (figure Rook White) (field 3 4) False) |>
                Expect.equal ["d4","e4","f4","g4","h4","b4","a4","c5","c6","c3"],
        test "freeDestinations 2" <| \_ ->
            (map showField <| freeDestinations GameStart <| figureMoves (figure Bishop White) (field 3 4) False) |>
                Expect.equal ["d5","e6","b5","a6","d3","b3"],
        test "captureDestinations 1" <| \_ ->
            (map showField <| captureDestinations GameStart <| figureMoves (figure Rook White) (field 3 4) False) |>
                Expect.equal ["c7"],
        test "captureDestinations 2" <| \_ ->
            (map showField <| captureDestinations GameStart <| figureMoves (figure Bishop White) (field 3 4) False) |>
                Expect.equal ["f7"]
    ]
