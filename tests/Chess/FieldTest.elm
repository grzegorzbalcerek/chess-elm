module Chess.FieldTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Field as Field exposing (..)
import Chess.Figure as Figure

suite : Test
suite =
    let c4 = {col = 3, row = 4}
        d8 = Field.field 4 8
    in
        describe "Field" [
            test "showField c4" <| \_ -> Expect.equal "c4" (Field.showField c4),
            test "showField d8" <| \_ -> Expect.equal "d8" (Field.showField d8),
            test "readField c4" <| \_ -> Expect.equal (Just c4) (Field.readField "c4"),
            test "readField aaa" <| \_ -> Expect.equal Nothing (Field.readField "aaa"),
            test "readField <empty>" <| \_ -> Expect.equal Nothing (Field.readField ""),
            test "Field.relative c4 1 1" <| \_ -> Expect.equal d8 (Field.relative c4 1 4),
            test "isLastRow c4 Black" <| \_ -> Expect.equal False (Field.isLastRow c4 Black),
            test "isLastRow d8 Black" <| \_ -> Expect.equal False (Field.isLastRow d8 Black),
            test "isLastRow d8 White" <| \_ -> Expect.equal True (Field.isLastRow d8 White),
            test "isValid d8" <| \_ -> Expect.equal True (Field.isValid d8),
            test "isValid e9" <| \_ -> Expect.equal False (Field.isValid (Field.relative d8 1 1))
        ]
