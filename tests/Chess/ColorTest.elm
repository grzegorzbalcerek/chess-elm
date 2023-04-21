module Chess.ColorTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Color as Color

suite : Test
suite =
    describe "Color" [
        describe "other" [
            test "turns black to white" <|
                \_ -> Expect.equal White (Color.other Black),
            test "turns white to black" <|
                \_ -> Expect.equal Black (Color.other White)
        ],
        describe "firstRow" [
            test "for Black is 8" <|
                \_ -> Expect.equal 8 (Color.firstRow Black),
            test "for White is 1" <|
                \_ -> Expect.equal 1 (Color.firstRow White)
        ]
    ]
