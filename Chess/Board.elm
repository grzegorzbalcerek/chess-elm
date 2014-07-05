module Chess.Board where

import Dict
import String
import Chess.Color (..)
import Chess.Figure (..)
import Chess.Field (..)
import Chess.Move (..)

type Board = Dict.Dict String Figure

{-| The board state when the game starts. -}
startingBoard : Board
startingBoard = Dict.fromList [
  (showField <| field 1 1, figure Rook White),
  (showField <| field 2 1, figure Knight White),
  (showField <| field 3 1, figure Bishop White),
  (showField <| field 4 1, figure Queen White),
  (showField <| field 5 1, figure King White),
  (showField <| field 6 1, figure Bishop White),
  (showField <| field 7 1, figure Knight White),
  (showField <| field 8 1, figure Rook White),
  (showField <| field 1 2, figure Pawn White),
  (showField <| field 2 2, figure Pawn White),
  (showField <| field 3 2, figure Pawn White),
  (showField <| field 4 2, figure Pawn White),
  (showField <| field 5 2, figure Pawn White),
  (showField <| field 6 2, figure Pawn White),
  (showField <| field 7 2, figure Pawn White),
  (showField <| field 8 2, figure Pawn White),
  (showField <| field 1 7, figure Pawn Black),
  (showField <| field 2 7, figure Pawn Black),
  (showField <| field 3 7, figure Pawn Black),
  (showField <| field 4 7, figure Pawn Black),
  (showField <| field 5 7, figure Pawn Black),
  (showField <| field 6 7, figure Pawn Black),
  (showField <| field 7 7, figure Pawn Black),
  (showField <| field 8 7, figure Pawn Black),
  (showField <| field 1 8, figure Rook Black),
  (showField <| field 2 8, figure Knight Black),
  (showField <| field 3 8, figure Bishop Black),
  (showField <| field 4 8, figure Queen Black),
  (showField <| field 5 8, figure King Black),
  (showField <| field 6 8, figure Bishop Black),
  (showField <| field 7 8, figure Knight Black),
  (showField <| field 8 8, figure Rook Black)]

{-| Shows the board. -}
showBoard : Board -> String
showBoard board =
  let showFieldContent row col = case Dict.get (showField <| field col row) board of
                                   Just x -> showFigure x
                                   Nothing -> "."
      showRow row = (String.show row) ++
                    String.concat (map (showFieldContent row) [1..8]) ++
                    (String.show row) ++ "\n"
  in
    " abcdefgh\n" ++ String.concat (map showRow <| reverse [1..8]) ++ " abcdefgh\n"

{-| Returns a new board, updated with a move. -}
updateBoard : Board -> Move -> Board
updateBoard board move = case move of
  RegularMove from to ->
    case Dict.get (showField from) board of
      Just figure -> Dict.insert (showField to) figure . Dict.remove (showField from) <| board
      _ -> board
  PromotionMove from to figure ->
    case Dict.get (showField from) board of
      Just _ -> Dict.insert (showField to) figure . Dict.remove (showField from) <| board
      _ -> board
  EnPassantMove from to captured ->
    case Dict.get (showField from) board of
      Just figure -> Dict.insert (showField to) figure .
                     Dict.remove (showField from) .
                     Dict.remove (showField captured) <| board
      _ -> board
  CastlingMove from to rookFrom rookTo ->
    case (Dict.get (showField from) board, Dict.get (showField rookFrom) board) of
      (Just king, Just rook) -> Dict.insert (showField to) king .
                                Dict.insert (showField rookTo) rook .
                                Dict.remove (showField from) .
                                Dict.remove (showField rookFrom) <| board
      _ -> board

getFigure : Board -> Field -> Maybe Figure
getFigure board field = Dict.get (showField field) board

showFigure' : Figure -> String
showFigure' figure =
  let symbol = String.toUpper <| showFigure figure
  in if symbol == "P" then " " else symbol

showMove : Move -> Board -> String
showMove move board = case move of
  RegularMove from to       -> concat [maybe " " showFigure' (getFigure board to), showField from, "-", showField to, " "]
  PromotionMove from to fig -> concat [maybe " " showFigure' (getFigure board to), showField from, "-", showField to, showFigure fig]
  EnPassantMove from to _   -> concat [maybe " " showFigure' (getFigure board to), showField from, "-", showField to, " "]
  CastlingMove _ {col} _ _  -> if col == 7 then "O-O    " else "O-O-O  "

{-

elm -m Chess\Board.elm
elm-repl
import Chess.Color (..)
import Chess.Board (..)
import Chess.Move (..)
import Chess.Field (..)
import Chess.Figure (..)
startingBoard
showBoard startingBoard
showBoard <| updateBoard startingBoard (RegularMove (field 2 2) (field 2 3))
showBoard <| updateBoard startingBoard (RegularMove (field 3 3) (field 2 3))
showBoard <| updateBoard startingBoard (PromotionMove (field 2 2) (field 2 8) (figure Queen White))
showBoard <| updateBoard startingBoard (EnPassantMove (field 2 2) (field 3 3) (field 3 7))
showBoard <| updateBoard startingBoard (CastlingMove (field 5 1) (field 3 1) (field 1 1) (field 4 1))
:exit

-}
