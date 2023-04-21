module Chess.Rank exposing (..)

import Dict exposing (Dict, fromList)
import List
import Dict
import String
import Chess.Color exposing (..)
import Chess.Figure exposing (..)
import Chess.Field exposing (..)
import Chess.FigureMoves exposing (..)
import Chess.Game exposing (..)

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
  let colRowRank cr = if cr >= 5 then 9 - cr else cr
  in 2*colRowRank(col) * colRowRank(row)

{-| Returns the figure rank based on the figures it is defending. -}
figureDefendingOtherFiguresRank : Game -> Field -> Figure -> Int
figureDefendingOtherFiguresRank game field figure =
  (List.length <| defendedDestinations game (figureMoves figure field True)) // 2

{-| Returns a rank value related to whether the King is under check or not. -}
checkRank : Game -> Color -> Int
checkRank game color =
  if (gameColor game == other color) && isKingUnderCheck game then 50 else 0

{-| Calculates the position rank taking one color into account. -}
colorRank : Game -> Color -> Int
colorRank game color =
  let ranks = (Dict.toList <| gameBoard game) |>
              List.filter (\(_,{figureColor}) -> figureColor == color) |>
              List.map (\(fieldStr,figure) ->
                case readField fieldStr of
                  Just field ->
                    figureRank figure + fieldRank field + figureDefendingOtherFiguresRank game field figure
                  Nothing -> 0
              )
  in List.sum ranks + checkRank game color

{-| Calculates the position rank from the point of view of a player. -}
rank : Game -> Color -> Int
rank game color = colorRank game color - colorRank game (other color)
