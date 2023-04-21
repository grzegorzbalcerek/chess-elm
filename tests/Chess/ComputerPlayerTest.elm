module Chess.ComputerPlayerTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Maybe
import Chess.Color exposing (Color(..))
import Chess.Color as Color
import Chess.Field as Field exposing (field)
import Chess.Figure as Figure exposing (figure, FigureType(..))
import Chess.Rank exposing (..)
import Chess.Game exposing (..)
import Chess.ComputerPlayer as CP exposing (..)

k1 = Maybe.withDefault GameStart <| makeMove (field 7 2) (field 7 4) Nothing GameStart
k2 = Maybe.withDefault GameStart <| makeMove (field 5 7) (field 5 6) Nothing k1
k3 = Maybe.withDefault GameStart <| makeMove (field 6 2) (field 6 4) Nothing k2
k4 = Maybe.withDefault GameStart <| makeMove (field 4 8) (field 8 4) Nothing k3

suite : Test
suite =
    describe "ComputerPlayer" [
        describe "moves" [
            test "List.length <| moves GameStart" <|
                \_ -> Expect.equal 2 (List.length <| moves GameStart),
            test "List.length <| moves k1" <|
                \_ -> Expect.equal 2 (List.length <| moves k1),
            test "List.length <| moves k2" <|
                \_ -> Expect.equal 2 (List.length <| moves k2),
            test "List.length <| moves k3" <|
                \_ -> Expect.equal 1 (List.length <| moves k3),
            test "List.length <| moves k4" <|
                \_ -> Expect.equal 0 (List.length <| moves k4)
        ]
    ]
