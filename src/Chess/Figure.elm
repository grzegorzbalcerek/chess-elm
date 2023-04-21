module Chess.Figure exposing (..)

import Chess.Color exposing (..)

{-| Represents chess figure types. -}
type FigureType = King | Queen | Rook | Bishop | Knight | Pawn

{-| Represents a figure, which has a type and a color. -}
type alias Figure = { figureType : FigureType, figureColor : Color }

{-| Construct a figure -}
figure : FigureType -> Color -> Figure
figure ft c = { figureType = ft, figureColor = c }

{-| Returns a one-character string representing the figure. -}
showFigure : Figure -> String
showFigure {figureType,figureColor} =
    case (figureType,figureColor) of
        (King, White) -> "k"
        (Queen, White) -> "q"
        (Rook, White) -> "r"
        (Bishop, White) -> "b"
        (Knight, White) -> "n"
        (Pawn, White) -> "p"
        (King, Black) -> "K"
        (Queen, Black) -> "Q"
        (Rook, Black) -> "R"
        (Bishop, Black) -> "B"
        (Knight, Black) -> "N"
        (Pawn, Black) -> "P"

{-| Returns a one-character string representing the figure as a unicode symbol. -}
showFigureUnicode : Figure -> String
showFigureUnicode {figureType,figureColor} =
    case (figureType,figureColor) of
        (King, White) -> "\u{2654}"
        (Queen, White) -> "\u{2655}"
        (Rook, White) -> "\u{2656}"
        (Bishop, White) -> "\u{2657}"
        (Knight, White) -> "\u{2658}"
        (Pawn, White) -> "\u{2659}"
        (King, Black) -> "\u{265a}"
        (Queen, Black) -> "\u{265b}"
        (Rook, Black) -> "\u{265c}"
        (Bishop, Black) -> "\u{265d}"
        (Knight, Black) -> "\u{265e}"
        (Pawn, Black) -> "\u{265f}"

{-

elm -m Chess\Figure.elm
elm-repl
import Chess.Figure (..)
import Chess.Color (..)
showFigure { figureType = King, figureColor = White } -- "k" : String
showFigure { figureType = King, figureColor = Black } -- "K" : String
showFigure { figureType = Queen, figureColor = Black } -- "Q" : String
showFigure { figureType = Rook, figureColor = Black } -- "R" : String
:exit

-}
