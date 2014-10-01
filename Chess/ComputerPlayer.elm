module Chess.ComputerPlayer where

import Dict
import Chess.Color (..)
import Chess.Figure (..)
import Chess.Field (..)
import Chess.Move (..)
import Chess.FigureMoves (..)
import Chess.Game (..)
import Chess.Rank (..)
import Chess.Util (..)

{-| Returns a sequence of the best ranked moves.-}
moves : Game -> [Game]
moves game =
  let moves = validGames game
  in if isEmpty moves
     then []
     else let
            rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy snd rankedMoves
            firstRank = snd << head <| rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

{-| Makes a move and returns the next game state. -}
generateMove : Game -> Maybe Game
generateMove game = case moves game of
  [] -> Nothing
  h::_ -> Just h

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
  
