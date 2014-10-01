module Chess.Figure where

import String (cons)
import Char (fromCode)
import Chess.Color (..)
import Chess.Color
type Color = Chess.Color.Color

{-| Represents chess figure types. -}
data FigureType = King | Queen | Rook | Bishop | Knight | Pawn

{-| Represents a figure, which has a type and a color. -}
type Figure = { figureType : FigureType, figureColor : Color }

{-| Construct a figure -}
figure : FigureType -> Color -> Figure
figure ft c = { figureType = ft, figureColor = c }

{-| Returns a one-character string representing the figure. -}
showFigure : Figure -> String
showFigure {figureType,figureColor} =
  if | figureType == King   && figureColor == White -> "k"
     | figureType == Queen  && figureColor == White -> "q"
     | figureType == Rook   && figureColor == White -> "r"
     | figureType == Bishop && figureColor == White -> "b"
     | figureType == Knight && figureColor == White -> "n"
     | figureType == Pawn   && figureColor == White -> "p"
     | figureType == King   && figureColor == Black -> "K"
     | figureType == Queen  && figureColor == Black -> "Q"
     | figureType == Rook   && figureColor == Black -> "R"
     | figureType == Bishop && figureColor == Black -> "B"
     | figureType == Knight && figureColor == Black -> "N"
     | figureType == Pawn   && figureColor == Black -> "P"

{-| Returns a one-character string representing the figure as a unicode symbol. -}
showFigureUnicode : Figure -> String
showFigureUnicode {figureType,figureColor} =
  let symbol = if | figureType == King   && figureColor == White -> 9812
                  | figureType == Queen  && figureColor == White -> 9813
                  | figureType == Rook   && figureColor == White -> 9814
                  | figureType == Bishop && figureColor == White -> 9815
                  | figureType == Knight && figureColor == White -> 9816
                  | figureType == Pawn   && figureColor == White -> 9817
                  | figureType == King   && figureColor == Black -> 9818
                  | figureType == Queen  && figureColor == Black -> 9819
                  | figureType == Rook   && figureColor == Black -> 9820
                  | figureType == Bishop && figureColor == Black -> 9821
                  | figureType == Knight && figureColor == Black -> 9822
                  | figureType == Pawn   && figureColor == Black -> 9823
  in cons (fromCode symbol) ""

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
