module Chess.FigureMoves exposing (..)

import Chess.Color as Color exposing (..)
import Chess.Figure as Figure exposing (..)
import Chess.Field as Field exposing (..)
import Chess.Util as Util exposing (takeWhile)
import List

{-| Sequences of relative figure positions for rook moves. -}
rookMoves : List (List Int, List Int)
rookMoves = [(List.range 1 8,List.repeat 8 0),
             (List.reverse (List.range -8 -1),List.repeat 8 0),
             (List.repeat 8 0,List.range 1 8),
             (List.repeat 8 0, List.reverse (List.range -8 -1))]

{-| Sequences of relative figure positions for bishop moves. -}
bishopMoves : List (List Int, List Int)
bishopMoves = [(List.range 1 8,List.range 1 8),
               (List.reverse (List.range -8 -1),List.range 1 8),
               (List.range 1 8,List.reverse (List.range -8 -1)),
               (List.reverse (List.range -8 -1),List.reverse (List.range -8 -1))]

{-| Sequences of relative figure positions for queen moves. -}
queenMoves : List (List Int, List Int)
queenMoves = rookMoves ++ bishopMoves

{-| Sequences of relative figure positions for knight moves. -}
knightMoves : List (List Int, List Int)
knightMoves = [([1],[2]),
               ([2],[1]),
               ([-1],[2]),
               ([2],[-1]),
               ([-1],[-2]),
               ([-2],[-1]),
               ([1],[-2]),
               ([-2],[1])]

{-| Sequences of relative figure positions for king moves. -}
kingMoves : List (List Int, List Int)
kingMoves = List.map (\(c,r) -> (List.take 1 c, List.take 1 r)) queenMoves

{-| Choose the sequences of relative figure positions
based on the figure position, type, color,
and whether the move is a capture move or not. -}
chooseFigureMoves : Figure -> Field -> Bool -> List (List Int, List Int)
chooseFigureMoves {figureType,figureColor} {row} capture =
  if figureType == Rook then rookMoves
  else if figureType == Bishop then bishopMoves
  else if figureType == King then kingMoves
  else if figureType == Queen then queenMoves
  else if figureType == Knight then knightMoves
  else if figureType == Pawn && figureColor == White && row == 2 && not capture then [([0,0],[1,2])]
  else if figureType == Pawn && figureColor == White && not capture then [([0],[1])]
  else if figureType == Pawn && figureColor == Black && row == 7 && not capture then [([0,0],[-1,-2])]
  else if figureType == Pawn && figureColor == Black && not capture then [([0],[-1])]
  else if figureType == Pawn && figureColor == White && capture then [([-1],[1]),([1],[1])]
  else if figureType == Pawn && figureColor == Black && capture then [([-1],[-1]),([1],[-1])]
  else []

{-| Returns the field relative to the given field according to
a pair of relative coordinates. -}
relativeField : Field -> (Int,Int) -> Field
relativeField {col,row} (c,r) = { col = col+c, row = row+r }

{-| Returns fields relative to the given field according to
the sequence of relative coordinates. -}
relativeFields : Field -> (List Int,List Int) -> List Field
relativeFields field (cols,rows) =
  takeWhile isValid (List.map (relativeField field) (List.map2 Tuple.pair cols rows))

{-| Returns possible figure moves.
The figure is on the field 'field' and the 'capture' flag indicate whether
the move is a capture. -}
figureMoves : Figure -> Field -> Bool -> List (List Field)
figureMoves figure field capture = List.map (relativeFields field) <| chooseFigureMoves figure field capture

{--
map (map showField) <| figureMoves (figure Pawn White) (field 2 2) True -- [[a3],[c3]]
map (map showField) <| figureMoves (figure Pawn White) (field 1 2) True -- [[],[b3]]
:exit

-}
