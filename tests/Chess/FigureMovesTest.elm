module Chess.FigureMovesTest exposing (..)

import Expect exposing (Expectation)
import List exposing (map)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Figure exposing (FigureType(..), Figure, figure)
import Chess.Field exposing (field, showField)
import Chess.FigureMoves exposing (..)

suite : Test
suite =
    describe "FigureMoves" [
        test "rookMoves" <| \_ ->
          chooseFigureMoves (figure Rook White) (field 1 2) False |> Expect.equal rookMoves,
        test "white pawn 1 2 False" <| \_ ->
          chooseFigureMoves (figure Pawn White) (field 1 2) False |> Expect.equal [([0,0],[1,2])],
        test "white pawn 1 4 False" <| \_ ->
          chooseFigureMoves (figure Pawn White) (field 1 4) False |> Expect.equal [([0],[1])],
        test "black pawn 1 7 False" <| \_ ->
          chooseFigureMoves (figure Pawn Black) (field 1 7) False |> Expect.equal [([0,0],[-1,-2])],
        test "black pawn 1 5 False" <| \_ ->
          chooseFigureMoves (figure Pawn Black) (field 1 5) False |> Expect.equal [([0],[-1])],
        test "white pawn 1 2 True" <| \_ ->
          chooseFigureMoves (figure Pawn White) (field 1 2) True |> Expect.equal [([-1],[1]),([1],[1])],
        test "white pawn 1 7 True" <| \_ ->
          chooseFigureMoves (figure Pawn Black) (field 1 7) True |> Expect.equal [([-1],[-1]),([1],[-1])],
        test "relativeField 1" <| \_ ->
          relativeField (field 1 2) (1,1) |> Expect.equal { col = 2, row = 3 },
        test "relativeField 2" <| \_ ->
          relativeField (field 1 2) (0,2) |> Expect.equal { col = 1, row = 4 },
        test "rook white 3 4" <| \_ ->
          (map (map showField) <| figureMoves (figure Rook White) (field 3 4) False)
          |> Expect.equal [["d4","e4","f4","g4","h4"],["b4","a4"],["c5","c6","c7","c8"],["c3","c2","c1"]],
        test "pawn white 2 2 False" <| \_ ->
          (map (map showField) <| figureMoves (figure Pawn White) (field 2 2) False)
          |> Expect.equal [["b3","b4"]],
        test "pawn white 2 2 True" <| \_ ->
          (map (map showField) <| figureMoves (figure Pawn White) (field 2 2) True)
          |> Expect.equal [["a3"],["c3"]],
        test "pawn white 1 2 True" <| \_ ->
          (map (map showField) <| figureMoves (figure Pawn White) (field 1 2) True)
          |> Expect.equal [[],["b3"]]
    ]
