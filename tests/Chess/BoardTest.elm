module Chess.BoardTest exposing (..)

import Expect exposing (Expectation)
import List exposing (map)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Figure exposing (FigureType(..), Figure, figure)
import Chess.Field exposing (field, showField)
import Chess.Board exposing (..)
import Chess.Move exposing (..)

suite : Test
suite =
    describe "Board" [
        test "startingBoard" <| \_ ->
          showBoard startingBoard |> Expect.equal " abcdefgh\n8RNBQKBNR8\n7PPPPPPPP7\n6........6\n5........5\n4........4\n3........3\n2pppppppp2\n1rnbqkbnr1\n abcdefgh\n",
        test "board 2" <| \_ ->
          showBoard (updateBoard startingBoard (RegularMove (field 2 2) (field 2 3))) |>
            Expect.equal " abcdefgh\n8RNBQKBNR8\n7PPPPPPPP7\n6........6\n5........5\n4........4\n3.p......3\n2p.pppppp2\n1rnbqkbnr1\n abcdefgh\n"
    ]
