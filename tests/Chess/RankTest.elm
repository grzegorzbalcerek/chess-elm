module Chess.RankTest exposing (..)

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

g1 = Maybe.withDefault GameStart <| makeMove (field 1 2) (field 1 3) Nothing GameStart
g2 = Maybe.withDefault GameStart <| makeMove (field 1 7) (field 1 6) Nothing g1
k1 = Maybe.withDefault GameStart <| makeMove (field 7 2) (field 7 4) Nothing GameStart
k2 = Maybe.withDefault GameStart <| makeMove (field 5 7) (field 5 6) Nothing k1
k3 = Maybe.withDefault GameStart <| makeMove (field 6 2) (field 6 4) Nothing k2
k4 = Maybe.withDefault GameStart <| makeMove (field 4 8) (field 8 4) Nothing k3

suite : Test
suite =
    describe "Rank" [
        describe "figureRank" [
            test "figureRank (Figure.figure Queen White)" <|
                \_ -> Expect.equal 900 (figureRank (figure Queen White)),
            test "figure Knight Black" <|
                \_ -> Expect.equal 300 (figureRank (figure Knight Black))
        ],
        describe "fieldRank" [
            test "fieldRank (Figure.field 1 1)" <|
                \_ -> Expect.equal 2 (fieldRank (field 1 1)),
            test "fieldRank (field 2 5)" <|
                \_ -> Expect.equal 16 (fieldRank (field 2 5)),
            test "fieldRank (field 4 4)" <|
                \_ -> Expect.equal 32 (fieldRank (field 4 4))
        ],
        describe "figureDefendingOtherFiguresRank" [
            test "figureDefendingOtherFiguresRank g2 (field 2 1) (figure Knight White)" <|
                \_ -> Expect.equal 1 (figureDefendingOtherFiguresRank g2 (field 2 1) (figure Knight White))
        ],
        describe "checkRank" [
            test "checkRank GameStart White" <|
                \_ -> Expect.equal 0 (checkRank GameStart White),
            test "checkRank k4 White" <|
                \_ -> Expect.equal 0 (checkRank k4 White),
            test "checkRank k4 Black" <|
                \_ -> Expect.equal 50 (checkRank k4 Black)
        ],
        describe "colorRank" [
            test "colorRank GameStart White" <|
                \_ -> Expect.equal 3928 (colorRank GameStart White),
            test "colorRank k1 White" <|
                \_ -> Expect.equal 3928 (colorRank k1 White),
            test "colorRank k2 White" <|
                \_ -> Expect.equal 3935 (colorRank k2 White),
            test "colorRank k3 White" <|
                \_ -> Expect.equal 3940 (colorRank k3 White),
            test "colorRank k4 White" <|
                \_ -> Expect.equal 3947 (colorRank k4 White)
        ],
        describe "rank" [
            test "rank GameStart White" <|
                \_ -> Expect.equal 8 (rank GameStart White),
            test "colorRank k1 White" <|
                \_ -> Expect.equal 0 (rank k1 White),
            test "rank k2 White" <|
                \_ -> Expect.equal 7 (rank k2 White),
            test "rank k3 White" <|
                \_ -> Expect.equal 5 (rank k3 White),
            test "rank k4 White" <|
                \_ -> Expect.equal -32 (rank k4 White),
            test "rank GameStart Black" <|
                \_ -> Expect.equal -8 (rank GameStart Black)
        ]
    ]
