module Chess.FigureTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Chess.Color exposing (Color(..))
import Chess.Figure exposing (FigureType(..), Figure)
import Chess.Figure as Figure

suite : Test
suite =
    describe "Figure" [
        describe "showFigure" [
            test "showFigure k" <| \_ -> Expect.equal "k" (Figure.showFigure (Figure.figure King White)),
            test "showFigure K" <| \_ -> Expect.equal "K" (Figure.showFigure (Figure.figure King Black)),
            test "showFigure q" <| \_ -> Expect.equal "q" (Figure.showFigure (Figure.figure Queen White)),
            test "showFigure R" <| \_ -> Expect.equal "R" (Figure.showFigure (Figure.figure Rook Black))
        ],
        describe "showFigureUnicode" [
            test "showFigureUnicode k" <| \_ -> Expect.equal "\u{2654}" (Figure.showFigureUnicode (Figure.figure King White)),
            test "showFigureUnicode K" <| \_ -> Expect.equal "\u{265a}" (Figure.showFigureUnicode (Figure.figure King Black)),
            test "showFigureUnicode q" <| \_ -> Expect.equal "\u{2655}" (Figure.showFigureUnicode (Figure.figure Queen White)),
            test "showFigureUnicode R" <| \_ -> Expect.equal "\u{265c}" (Figure.showFigureUnicode (Figure.figure Rook Black))
        ]
    ]
