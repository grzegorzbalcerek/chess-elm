module Chess.Rank where

import Dict
import Chess.Color (..)
import Chess.Figure (..)
import Chess.Field (..)
import Chess.Move (..)
import Chess.FigureMoves (..)
import Chess.Game (..)
import Chess.Util (..)

{-| Returns the rank of a figure of the given type. -}
figureRank : Figure -> Int
figureRank {figureType} = case figureType of
  Queen -> 900
  Rook -> 450
  Knight -> 300
  Bishop -> 300
  Pawn -> 100
  _ -> 0

{-|  Returns the rank of the given field. -}
fieldRank : Field -> Int
fieldRank {col,row} =
  let colRowRank cr = if cr>=5 then 9-cr else cr
  in 2*colRowRank(col) * colRowRank(row)

{-| Returns the figure rank based on the figures it is defending. -}
figureDefendingOtherFiguresRank : Game -> Field -> Figure -> Int
figureDefendingOtherFiguresRank game field figure =
  (length <| defendedDestinations game (figureMoves figure field True)) `div` 2

{-| Returns a rank value related to whether the King is under check or not. -}
checkRank : Game -> Color -> Int
checkRank game color =
  if (gameColor game == other color) && isKingUnderCheck game then 50 else 0

{-| Calculates the position rank taking one color into account. -}
colorRank : Game -> Color -> Int
colorRank game color =
  let ranks =
    (Dict.toList <| gameBoard game) |>
    filter (\(_,{figureColor}) -> figureColor == color) |>
    map (\(fieldStr,figure) ->
      let field = readField fieldStr
          r1 = figureRank figure
          r2 = fieldRank field
          r3 = figureDefendingOtherFiguresRank game field figure
      in r1+r2+r3)
  in sum ranks + checkRank game color

{-| Calculates the position rank from the point of view of a player. -}
rank : Game -> Color -> Int
rank game color = colorRank game color - colorRank game (other color)

{-

elm -m Chess/Rank.elm
elm-repl
import Chess.Rank (..)
import Chess.Color (..)
import Chess.Field (..)
import Chess.Figure (..)
import Chess.Game (..)
figureRank (figure Queen White) -- 900
figureRank (figure Knight Black) -- 300
fieldRank (field 1 1) -- 2
fieldRank (field 2 5) -- 16
fieldRank (field 4 4) -- 32
g1 = maybe GameStart id <| move (field 1 2) (field 1 3) Nothing GameStart
g2 = maybe GameStart id <| move (field 1 7) (field 1 6) Nothing g1
figureDefendingOtherFiguresRank g2 (field 2 1) (figure Knight White) -- 1
g1 = maybe GameStart id <| move (field 7 2) (field 7 4) Nothing GameStart
g2 = maybe GameStart id <| move (field 5 7) (field 5 6) Nothing g1
g3 = maybe GameStart id <| move (field 6 2) (field 6 4) Nothing g2
g4 = maybe GameStart id <| move (field 4 8) (field 8 4) Nothing g3
checkRank GameStart White -- 0
checkRank g4 White -- 0
checkRank g4 Black -- 50
colorRank GameStart White -- 3928
colorRank g1 White -- 3928
colorRank g2 White -- 3935
colorRank g3 White -- 3940
colorRank g4 White -- 3947
rank GameStart White -- 8
rank g1 White -- 0
rank g2 White -- 7
rank g3 White -- 5
rank g4 White -- -32
rank GameStart Black -- -8
:exit

-}
