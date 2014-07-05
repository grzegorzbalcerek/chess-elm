module Chess.FigureMoves where

import Chess.Color (..)
import Chess.Figure (..)
import Chess.Field (..)
import Chess.Util (takeWhile)

{-| Sequences of relative figure positions for rook moves. -}
rookMoves : [([Int],[Int])]
rookMoves = [([1..8],repeat 8 0),
             (reverse [(-8)..(-1)],repeat 8 0),
             (repeat 8 0,[1..8]),
             (repeat 8 0, reverse [(-8)..(-1)])]

{-| Sequences of relative figure positions for bishop moves. -}
bishopMoves : [([Int],[Int])]
bishopMoves = [([1..8],[1..8]),
               (reverse [(-8)..(-1)],[1..8]),
               ([1..8],reverse [(-8)..(-1)]),
               (reverse [(-8)..(-1)],reverse [(-8)..(-1)])]

{-| Sequences of relative figure positions for queen moves. -}
queenMoves : [([Int],[Int])]
queenMoves = rookMoves ++ bishopMoves

{-| Sequences of relative figure positions for knight moves. -}
knightMoves : [([Int],[Int])]
knightMoves = [([1],[2]),
               ([2],[1]),
               ([-1],[2]),
               ([2],[-1]),
               ([-1],[-2]),
               ([-2],[-1]),
               ([1],[-2]),
               ([-2],[1])]

{-| Sequences of relative figure positions for king moves. -}
kingMoves : [([Int],[Int])]
kingMoves = map (\(c,r) -> (take 1 c, take 1 r)) queenMoves

{-| Choose the sequences of relative figure positions
based on the figure position, type, color,
and whether the move is a capture move or not. -}
chooseFigureMoves : Figure -> Field -> Bool -> [([Int],[Int])]
chooseFigureMoves {figureType,figureColor} {row} capture =
  if | figureType == Rook -> rookMoves
     | figureType == Bishop -> bishopMoves
     | figureType == King -> kingMoves
     | figureType == Queen -> queenMoves
     | figureType == Knight -> knightMoves
     | figureType == Pawn && figureColor == White && row == 2 && not capture -> [([0,0],[1,2])]
     | figureType == Pawn && figureColor == White && not capture -> [([0],[1])]
     | figureType == Pawn && figureColor == Black && row == 7 && not capture -> [([0,0],[-1,-2])]
     | figureType == Pawn && figureColor == Black && not capture -> [([0],[-1])]
     | figureType == Pawn && figureColor == White && capture -> [([-1],[1]),([1],[1])]
     | figureType == Pawn && figureColor == Black && capture -> [([-1],[-1]),([1],[-1])]

{-| Returns the field relative to the given field according to
a pair of relative coordinates. -}
relativeField : Field -> (Int,Int) -> Field
relativeField {col,row} (c,r) = { col = col+c, row = row+r }

{-| Returns fields relative to the given field according to
the sequence of relative coordinates. -}
relativeFields : Field -> ([Int],[Int]) -> [Field]
relativeFields field (cols,rows) =
  takeWhile isValid (map (relativeField field) (zip cols rows))

{-| Returns possible figure moves.
The figure is on the field 'field' and the 'capture' flag indicate whether
the move is a capture. -}
figureMoves : Figure -> Field -> Bool -> [[Field]]
figureMoves figure field capture = map (relativeFields field) <| chooseFigureMoves figure field capture

{--

elm -m Chess\FigureMoves.elm
elm-repl
import Chess.Color (..)
import Chess.Field (..)
import Chess.Figure (..)
import Chess.FigureMoves (..)
rookMoves
bishopMoves
queenMoves
knightMoves
kingMoves
chooseFigureMoves (figure Rook White) (field 1 2) False -- rookMoves
chooseFigureMoves (figure Pawn White) (field 1 2) False -- [([0,0],[1,2])]
chooseFigureMoves (figure Pawn White) (field 1 4) False -- [([0],[1])]
chooseFigureMoves (figure Pawn Black) (field 1 7) False -- [([0,0],[-1,-2])]
chooseFigureMoves (figure Pawn Black) (field 1 5) False -- [([0],[-1])]
chooseFigureMoves (figure Pawn White) (field 1 2) True -- [([-1],[1]),([1],[1])]
chooseFigureMoves (figure Pawn Black) (field 1 7) True -- [([-1],[-1]),([1],[-1])]
relativeField (field 1 2) (1,1) -- { col = 2, row = 3 }
relativeField (field 1 2) (0,2) -- { col = 1, row = 4 }
relativeFields (field 2 2) ([0,0],[1,2]) -- [{ col = 2, row = 3 },{ col = 2, row = 4 }]
map (map showField) <| figureMoves (figure Rook White) (field 3 4) False -- [[d4,e4,f4,g4,h4],[b4,a4],[c5,c6,c7,c8],[c3,c2,c1]]
map (map showField) <| figureMoves (figure Pawn White) (field 2 2) False -- [[b3,b4]]
map (map showField) <| figureMoves (figure Pawn White) (field 2 2) True -- [[a3],[c3]]
map (map showField) <| figureMoves (figure Pawn White) (field 1 2) True -- [[],[b3]]
:exit

-}
