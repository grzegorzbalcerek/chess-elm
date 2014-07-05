module Chess.Game where

import String
import Dict
import Chess.Color (..)
import Chess.Figure (..)
import Chess.Field (..)
import Chess.Move
import Chess.Move (..)
import Chess.FigureMoves (..)
import Chess.Board (Board,startingBoard,updateBoard,showBoard,showMove)
import Chess.Util (..)

data Game = GameStart | OngoingGame Color Board [Game] Move

gameMoves : Game -> [String]
gameMoves game = (game::(gameHist game)) >>= \g ->
  case g of
    GameStart -> []
    OngoingGame _ board _ move -> [showMove move board]

showGameHist : Game -> String
showGameHist game =
  let moves = gameMoves game
      separator num = if num `mod` 2 == 0 then " " else "\n"
      moveNumber num = if num `mod` 2 == 0 then String.concat [String.padLeft 3 ' ' (show (1 + num `div` 2)),". "] else ""
      step move (num,str) = (num+1, String.concat [str, moveNumber num, move, separator num])
      (num,str) = foldr step (0,"") moves
  in
    str

gameMessage : Game -> String
gameMessage game = case game of
  GameStart -> "White to begin"
  OngoingGame color board _ lastMove ->
    "Last move: " ++ (showColor <| other color) ++ " " ++
    (showField <| Chess.Move.from lastMove) ++ " to " ++
    (showField <| Chess.Move.to lastMove)

showGame : Game -> String
showGame game = String.concat [gameMessage game, "\n", showBoard (gameBoard game)]

gameColor game = case game of
  GameStart -> White
  OngoingGame color _ _ _ -> color

gameHist game = case game of
  GameStart -> []
  OngoingGame _ _ hist _ -> hist

gameBoard game = case game of
  GameStart -> startingBoard
  OngoingGame _ board _ _ -> board

{-| Verifies if the given field is empty. -}
isFieldEmpty game field = not (Dict.member (showField field) (gameBoard game))

{-| Returns free fields onto which the figure may be moved. -}
freeDestinations : Game -> [[Field]] -> [Field]
freeDestinations game fieldss =
  concatMap (\fields -> takeWhile (isFieldEmpty game) fields) fieldss

{-| Returns fields occupied by the enemy figures
(including the case when that figure is the King)
onto which the figure may be moved. -}
captureDestinations : Game -> [[Field]] -> [Field]
captureDestinations game =
  let hasEnemyFigure field =
        (mapMaybe .figureColor (Dict.get (showField field) (gameBoard game))) == (Just <| other (gameColor game))
  in
    concatMap <| filter hasEnemyFigure .
                 take 1 .
                 dropWhile (isFieldEmpty game)

{-| Returns fields occupied by the enemy figures
(including the case when that figure is the King)
onto which the figure may be moved. -}
defendedDestinations : Game -> [[Field]] -> [Field]
defendedDestinations game =
  let hasSameColorFigure field =
        (mapMaybe .figureColor (Dict.get (showField field) (gameBoard game))) == Just (gameColor game)
  in
    concatMap <| filter hasSameColorFigure .
                 take 1 .
                 dropWhile (isFieldEmpty game)

{-| Returns a new game, updated with a move. -}
updateGame : Game -> Move -> Game
updateGame game move =
  OngoingGame (other (gameColor game))
              (updateBoard (gameBoard game) move)
              (game::(gameHist game))
              move

{-| Verifies if the enemy King is under check. -}
isOtherKingUnderCheck game =
  let isKingOnBoard g = any (\x -> x == figure King (other <| gameColor game))
                            (Dict.values (gameBoard g))
  in
    not <| all isKingOnBoard (nextGames game)

{-| Verifies if the King of the player who is about to make a move is under check. -}
isKingUnderCheck : Game -> Bool
isKingUnderCheck game =
  let newGame = OngoingGame (other <| gameColor game)
                            (gameBoard game)
                            (game::(gameHist game))
                            (RegularMove (field 0 0) (field 0 0))
  in isOtherKingUnderCheck newGame

{-| Verifies the conditions of when the castling move is permitted:
-- whether the King and the Rook are on their initial positions,
-- whether they were there from the begining of the game,
-- whether the fields between them are free and
-- whether the field to be passed by the King is checked or not.
-- If the given castling move is permitted, the method returns a one-element sequence.
-- Otherwise it returns an empty sequence. -}
castling game kingTo rookFrom rookTo otherCol =
  let color = gameColor game
      row = firstRow color
      hist = gameHist game
      board = gameBoard game
  in if Dict.get (showField <| field 5 row) board == Just (figure King color) &&
        Dict.get (showField <| field rookFrom row) board == Just (figure Rook color) &&
        Dict.get (showField <| field rookTo row) board == Nothing &&
        Dict.get (showField <| field kingTo row) board == Nothing &&
        Dict.get (showField <| field otherCol row) board == Nothing &&
        all (\g -> Dict.get (showField <| field 5 row) (gameBoard g) == Just (figure King color)) hist &&
        all (\g -> Dict.get (showField <| field rookFrom row) (gameBoard g) == Just (figure Rook color)) hist &&
        not (isOtherKingUnderCheck (updateGame game (RegularMove (field 5 row) (field rookTo row))))
     then [updateGame game (CastlingMove (field 5 row) (field kingTo row) (field rookFrom row) (field rookTo row))]
     else []

{-| Verifies if the en passant capture move is possible. -}
isEnPassantCapture game from to = case game of
  GameStart -> False
  OngoingGame color board _ lastMove ->
    Dict.get (showField <| Chess.Move.to lastMove) board == Just (Figure Pawn (other color)) &&
    Chess.Move.to lastMove == field (.col to) (.row from) &&
    Chess.Move.from lastMove == field (.col to) (.row from + 2 * (.row to - .row from))

-- | Returns next games after possible next moves moves (including those
-- moves after which the King is checked).
-- The code itereates over all figures that have the same color as
-- the color of the next move. The 'g' value contains sequences of game states
-- corresponding to the possible next moves of the given figure.
-- Figure moves depend on its kind. The Rook, the Knight, the Queen, the Bishop
-- and the King are treated in a similar way, except for the King, for which
-- the castling moves are included as well.
-- Potentially there are two possible castling moves.
-- Each of them is handled by a call to the 'castling' method.
-- The most complex case handled by the mthod is the case of the Pawn moves.
-- The Pawn may move forward onto a free field or forward and left or right onto
-- a field occupied by an enemy figure. In both cases, if the destination field
-- lies on the last row, the set of possible moves includes the possible
-- promotions to other figures. In addition to that, the Pawn may make the so
-- called en passant capture, which consists of moving the Pawn forward and left
-- or right onto a free field which has been passed-by by an enemy Pawn in the
-- previous move.
nextGames : Game -> [Game]
nextGames game =
    filter (\(_,{figureColor}) -> figureColor == gameColor game) (Dict.toList <| gameBoard game) >>= \(fromStr,fig) ->
    let figType = .figureType fig
        from = readField fromStr
    in
      if figType /= Pawn
      then
        (let fieldss = figureMoves fig from True
         in map (\to -> updateGame game (RegularMove from to)) <|
            freeDestinations game fieldss ++ captureDestinations game fieldss) ++
        (if figType == King
         then castling game 3 1 4 2 ++ castling game 7 8 6 7
         else [])
      else
        let regularAndPromotionMoves =
              concat [ freeDestinations game (figureMoves fig from False)
                     , captureDestinations game (figureMoves fig from True) ] >>=
                \to -> if isLastRow to (gameColor game)
                       then map (\figType -> updateGame game <| PromotionMove from to <|
                                               figure figType <| gameColor game)
                            [ Queen, Rook, Bishop, Knight ]
                       else [ updateGame game (RegularMove from to) ]
            enPassantMoves =
              map (\to -> updateGame game (EnPassantMove from to (field (.col to) (.row from)))) .
              filter (isEnPassantCapture game from) <|
              freeDestinations game (figureMoves fig from True)
        in regularAndPromotionMoves ++ enPassantMoves


{-| Filters out the next games in which the king is under check. -}
validGames : Game -> [Game]
validGames game = filter (not . isOtherKingUnderCheck) <| nextGames game

{-| Returns a list of valid promotion figures. -}
validPromotionsMoves : Game -> Field -> Field -> [Figure]
validPromotionsMoves game from to =
  if isLastRow to (gameColor game) &&
     mapMaybe .figureType (Dict.get (showField from) (gameBoard game)) == Just Pawn
  then validGames game >>= \nextGame ->
       case nextGame of
         OngoingGame _ _ _ (PromotionMove f t fig) -> if f == from && t == to then [fig] else []
         _ -> []
  else []

{-| Verifies if the game is over.
The following end game conditions are handled:
+ after every possible move the King is under check,
+ only the two Kings are left on the board,
+ only the two Kings, one Bishop and one Knight are left on the board,
+ only the two Kings and two Knights of the same color are left on the board,
+ the same position occurred three times. -}
isGameFinished game =
  all isOtherKingUnderCheck (nextGames game) ||
  elem (map showFigure <| Dict.values (gameBoard game))
       [ sort [ "k", "K" ],
         sort [ "k", "K" , "b" ],
         sort [ "k", "K" , "B" ],
         sort [ "k", "K" , "n" ],
         sort [ "k", "K" , "N" ],
         sort [ "k", "K" , "n" , "n" ],
         sort [ "k", "K" , "N" , "N" ]] ||
  (not . isEmpty . filter (\g -> length g >= 3) . groupElements . sort . map (showBoard . gameBoard) <| game :: gameHist game)

{-| Returns 'Just' containing the color of the game winner or 'Nothing' if there is no winner. -}
winner game =
  if (isGameFinished game && isKingUnderCheck game)
  then Just . other . gameColor <| game
  else Nothing

{-| Returns a new game state after moving a figure. If the given
move is not possible, it returns Nothing. -}
makeMove : Field -> Field -> Maybe Figure -> Game -> Maybe Game
makeMove from to promotion game =
  let isMatching (OngoingGame _ _ _ move) =
          case move of
            RegularMove f t -> f == from && t == to && isNothing promotion
            PromotionMove f t fig -> f == from && t == to && promotion == Just fig
            EnPassantMove f t _ -> f == from && t == to && isNothing promotion
            CastlingMove f t _ _ ->  f == from && t == to && isNothing promotion
  in
    find (always True) <| filter isMatching <| validGames game

{-

elm -m Chess\Game.elm
elm-repl
import Chess.Color (..)
import Chess.Board (..)
import Chess.Move (..)
import Chess.Field (..)
import Chess.Figure (..)
import Chess.FigureMoves (..)
import Chess.Game (..)
import Chess.Util (..)
gameMessage GameStart
showGame GameStart
isFieldEmpty GameStart (field 2 2) -- False
isFieldEmpty GameStart (field 2 3) -- True
map showField <| freeDestinations GameStart <| figureMoves (figure Rook White) (field 3 4) False -- [d4,e4,f4,g4,h4,b4,a4,c5,c6,c3]
map showField <| freeDestinations GameStart <| figureMoves (figure Bishop White) (field 3 4) False -- [d5,e6,b5,a6,d3,b3]
map showField <| captureDestinations GameStart <| figureMoves (figure Rook White) (field 3 4) False -- [c7]
map showField <| captureDestinations GameStart <| figureMoves (figure Bishop White) (field 3 4) False -- [f7]
isOtherKingUnderCheck GameStart -- False
isKingUnderCheck GameStart -- False
isGameFinished GameStart -- False
winner GameStart -- Nothing
length (nextGames GameStart) -- 20
length (validGames GameStart) -- 20
makeMove (field 1 2) (field 1 5) Nothing GameStart -- Nothing
g1 = makeMove (field 7 2) (field 7 4) Nothing GameStart
mapMaybe gameMessage g1
mapMaybe showGame g1
g2 = maybe Nothing (\g ->  makeMove (field 5 7) (field 5 6) Nothing g) g1
mapMaybe showGame g2
g3 = maybe Nothing (\g ->  makeMove (field 6 2) (field 6 4) Nothing g) g2
mapMaybe showGame g3
g4 = maybe Nothing (\g ->  makeMove (field 4 8) (field 8 4) Nothing g) g3
mapMaybe showGame g4
mapMaybe isOtherKingUnderCheck g4 -- Just False
mapMaybe isKingUnderCheck g4 -- Just True
mapMaybe isGameFinished g4 -- Just True
mapMaybe winner g4 -- Just (Just Black)
mapMaybe length (mapMaybe nextGames g4) -- Just 20
mapMaybe length (mapMaybe validGames g4) -- Just 0
mapMaybe gameMoves g4

import Chess.Util (..)
import Chess.Field (..)
import Chess.Game (..)
g1 = maybe GameStart id <| makeMove (readField "g1") (readField "f3") Nothing GameStart
g2 = maybe GameStart id <| makeMove (readField "a7") (readField "a6") Nothing g1
g3 = maybe GameStart id <| makeMove (readField "g2") (readField "g3") Nothing g2
g4 = maybe GameStart id <| makeMove (readField "a6") (readField "a5") Nothing g3
g5 = maybe GameStart id <| makeMove (readField "f1") (readField "g2") Nothing g4
g6 = maybe GameStart id <| makeMove (readField "a5") (readField "a4") Nothing g5
showGame g6
map showGame (nextGames g6)
g7 = maybe GameStart id <| makeMove (readField "e1") (readField "g1") Nothing g6

import Chess.Util (..)
import Chess.Field (..)
import Chess.Game (..)
g1 = maybe GameStart id <| makeMove (readField "e2") (readField "e4") Nothing GameStart
g2 = maybe GameStart id <| makeMove (readField "a7") (readField "a6") Nothing g1
g3 = maybe GameStart id <| makeMove (readField "e4") (readField "e5") Nothing g2
g4 = maybe GameStart id <| makeMove (readField "d7") (readField "d5") Nothing g3
showGame g4
map showGame (nextGames g4)
g5 = maybe GameStart id <| makeMove (readField "e5") (readField "d6") Nothing g4
showGame g5

:exit

-}
