module Chess.ComputerPlayer exposing (..)

import Dict
import Chess.Color exposing (..)
import Chess.Figure exposing (..)
import Chess.Field exposing (..)
import Chess.Move exposing (..)
import Chess.FigureMoves exposing (..)
import Chess.Game exposing (..)
import Chess.Rank exposing (..)
import Chess.Util exposing (..)

{-| Returns a sequence of the best ranked moves.-}
moves : Game -> List Game
moves game =
  let moves1 = validGames game
  in if List.isEmpty moves1
     then []
     else let
            rankedMoves = List.map (\g -> (g, rank g (gameColor g))) moves1
            rankedMovesSorted = List.sortBy Tuple.second rankedMoves
            firstRank = case rankedMovesSorted of
                           [] -> -9999999
                           x :: xs -> Tuple.second x
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in List.map Tuple.first maxRankMoves

{-| Makes a move and returns the next game state. -}
generateMove : Game -> Maybe Game
generateMove game =
  List.head <| moves game

{-

elm -m Chess/ComputerPlayer.elm
elm-repl
import Chess.ComputerPlayer (..)
import Chess.Game (..)
import Chess.Field (..)
g1 = maybe GameStart id <| move (field 7 2) (field 7 4) Nothing GameStart
g2 = maybe GameStart id <| move (field 5 7) (field 5 6) Nothing g1
g3 = maybe GameStart id <| move (field 6 2) (field 6 4) Nothing g2
g4 = maybe GameStart id <| move (field 4 8) (field 8 4) Nothing g3
length <| moves GameStart -- 2
length <| moves g1 -- 2
length <| moves g2 -- 2
length <| moves g3 -- 1
length <| moves g4 -- 0
g1 = maybe GameStart id <| generateMove GameStart
g2 = maybe GameStart id <| generateMove g1
g3 = maybe GameStart id <| generateMove g2
g4 = maybe GameStart id <| generateMove g3
showGame g4
:exit

-}
  
