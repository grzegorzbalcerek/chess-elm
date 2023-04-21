module Chess.Game exposing (..)

import String
import Dict
import Maybe exposing (..)
import Chess.Color exposing (..)
import Chess.Figure exposing (..)
import Chess.Field exposing (..)
import Chess.Move
import Chess.Move exposing (..)
import Chess.FigureMoves exposing (..)
import Chess.Board exposing (Board,startingBoard,updateBoard,showBoard,showMove)
import Chess.Util as Util exposing (..)
import Chess.Color exposing (..)

type Game = GameStart | OngoingGame Color Board Game Move

gameMessage : Game -> String
gameMessage game = case game of
  GameStart -> "White to begin"
  OngoingGame color board _ lastMove ->
    "Last move: " ++ (showColor <| other color) ++ " " ++
    (showField <| Chess.Move.from lastMove) ++ " to " ++
    (showField <| Chess.Move.to lastMove)

gameMoves : Game -> List String
gameMoves game =
  case game of
    GameStart -> []
    OngoingGame _ board prevGame move -> (showMove move board) :: gameMoves prevGame

showGameHist : Game -> String
showGameHist game =
  let moves = gameMoves game
      separator num1 = if num1 // 2 == 0 then " " else "\n"
      moveNumber num2 = if num2 // 2 == 0 then String.concat [String.padLeft 3 ' ' (String.fromInt (1 + num2 // 2)),". "] else ""
      step move (num3,str) = (num3+1, String.concat [str, moveNumber num3, move, separator num3])
      (n,result) = List.foldr step (0,"") moves
  in
    result

showGame : Game -> String
showGame game = String.concat [gameMessage game, "\n", showBoard (gameBoard game)]

gameColor game = case game of
  GameStart -> White
  OngoingGame color _ _ _ -> color

gameHist game = case game of
  GameStart -> []
  OngoingGame _ _ prevGame _ -> game :: gameHist prevGame

gameBoard game = case game of
  GameStart -> startingBoard
  OngoingGame _ board _ _ -> board

{-| Verifies if the given field is empty. -}
isFieldEmpty game field = not (Dict.member (showField field) (gameBoard game))

{-| Returns free fields onto which the figure may be moved. -}
freeDestinations : Game -> List (List Field) -> List Field
freeDestinations game fieldss =
  List.concatMap (\fields -> takeWhile (isFieldEmpty game) fields) fieldss

{-| Returns fields occupied by the enemy figures
(including the case when that figure is the King)
onto which the figure may be moved. -}
captureDestinations : Game -> List (List Field) -> List Field
captureDestinations game =
  let hasEnemyFigure field =
        (Maybe.map .figureColor (Dict.get (showField field) (gameBoard game))) == (Just <| other (gameColor game))
  in
    List.concatMap <| List.filter hasEnemyFigure <<
                 List.take 1 <<
                 dropWhile (isFieldEmpty game)

{-| Returns fields occupied by the enemy figures
(including the case when that figure is the King)
onto which the figure may be moved. -}
defendedDestinations : Game -> List (List Field) -> List Field
defendedDestinations game =
  let hasSameColorFigure field =
        (Maybe.map .figureColor (Dict.get (showField field) (gameBoard game))) == Just (gameColor game)
  in
    List.concatMap <| List.filter hasSameColorFigure <<
                 List.take 1 <<
                 dropWhile (isFieldEmpty game)

--{-| Returns a new game, updated with a move. -}
updateGame : Game -> Move -> Game
updateGame game move =
  OngoingGame (other (gameColor game))
              (updateBoard (gameBoard game) move)
              game
              move
--
{-| Verifies if the enemy King is under check. -}
isOtherKingUnderCheck game =
  let isKingOnBoard g = List.any (\x -> x == figure King (other <| gameColor game))
                            (Dict.values (gameBoard g))
  in
    not <| List.all isKingOnBoard (nextGames game)

{-| Verifies if the King of the player who is about to make a move is under check. -}
isKingUnderCheck : Game -> Bool
isKingUnderCheck game =
  let newGame = OngoingGame (other <| gameColor game)
                            (gameBoard game)
                            game
                            (RegularMove (field 0 0) (field 0 0))
  in isOtherKingUnderCheck newGame

{-| Verifies the conditions of when the castling move is permitted:
-- whether the King and the Rook are on their initial positions,
-- whether they were there from the beginning of the game,
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
        List.all (\g -> Dict.get (showField <| field 5 row) (gameBoard g) == Just (figure King color)) hist &&
        List.all (\g -> Dict.get (showField <| field rookFrom row) (gameBoard g) == Just (figure Rook color)) hist &&
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
-- The code iterates over all figures that have the same color as
-- the color of the next move. The 'g' value contains sequences of game states
-- corresponding to the possible next moves of the given figure.
-- Figure moves depend on its kind. The Rook, the Knight, the Queen, the Bishop
-- and the King are treated in a similar way, except for the King, for which
-- the castling moves are included as well.
-- Potentially there are two possible castling moves.
-- Each of them is handled by a call to the 'castling' method.
-- The most complex case handled by the method is the case of the Pawn moves.
-- The Pawn may move forward onto a free field or forward and left or right onto
-- a field occupied by an enemy figure. In both cases, if the destination field
-- lies on the last row, the set of possible moves includes the possible
-- promotions to other figures. In addition to that, the Pawn may make the so
-- called en passant capture, which consists of moving the Pawn forward and left
-- or right onto a free field which has been passed-by by an enemy Pawn in the
-- previous move.
nextGames : Game -> List Game
nextGames game =
    Dict.toList (gameBoard game) |>
        List.filter (\(_,{figureColor}) -> figureColor == gameColor game) |>
        List.map (\(fromStr,fig) -> maybeNextGamesAfterMoves game fromStr fig) |>
        List.concat

maybeNextGamesAfterMoves : Game -> String -> Figure -> List Game
maybeNextGamesAfterMoves game fromStr figure =
    case readField fromStr of
        Just from -> nextGamesAfterMoves game from figure
        Nothing -> []

nextGamesAfterMoves : Game -> Field -> Figure -> List Game
nextGamesAfterMoves game from figure =
  if .figureType figure == Pawn
  then
    nextGamesAfterPawnRegularAndPromotionMoves game from figure ++ nextGamesAfterPawnEnPassantMoves game from figure
  else
    nextGamesAfterFigureMoves game from figure

nextGamesAfterFigureMoves : Game -> Field -> Figure -> List Game
nextGamesAfterFigureMoves game from fig =
    (let fieldss = figureMoves fig from True
     in List.map (\to -> updateGame game (RegularMove from to)) <|
        freeDestinations game fieldss ++ captureDestinations game fieldss) ++
    (if .figureType fig == King
     then castling game 3 1 4 2 ++ castling game 7 8 6 7
     else [])

nextGamesAfterPawnRegularAndPromotionMoves : Game -> Field -> Figure -> List Game
nextGamesAfterPawnRegularAndPromotionMoves game from fig =
  (List.concat (List.map (\to -> if isLastRow to (gameColor game)
                   then List.map (\ft -> updateGame game (PromotionMove from to ft)) [
                     figure Queen (gameColor game), figure Rook (gameColor game), figure Bishop (gameColor game), figure Knight (gameColor game)
                   ]
                   else [ updateGame game (RegularMove from to) ])
            (List.concat [ freeDestinations game (figureMoves fig from False), captureDestinations game (figureMoves fig from True) ])))


nextGamesAfterPawnEnPassantMoves : Game -> Field -> Figure -> List Game
nextGamesAfterPawnEnPassantMoves game from fig =
  List.map (\to -> updateGame game (EnPassantMove from to (field (.col to) (.row from))))
    (List.filter (isEnPassantCapture game from) <| freeDestinations game (figureMoves fig from True))


{-| Filters out the next games in which the king is under check. -}
validGames : Game -> List Game
validGames game = List.filter (not << isOtherKingUnderCheck) <| nextGames game

{-| Returns a list of valid promotion figures. -}
validPromotionsMoves : Game -> Field -> Field -> List Figure
validPromotionsMoves game from to =
  if isLastRow to (gameColor game) &&
     Maybe.map .figureType (Dict.get (showField from) (gameBoard game)) == Just Pawn
  then
   validGames game |>
    List.map (\nextGame ->
       case nextGame of
         OngoingGame _ _ _ (PromotionMove f t fig) -> if f == from && t == to then [fig] else []
         _ -> [] ) |>
    List.concat
  else []

{-| Verifies if the game is over.
The following end game conditions are handled:
+ after every possible move the King is under check,
+ only the two Kings are left on the board,
+ only the two Kings, one Bishop and one Knight are left on the board,
+ only the two Kings and two Knights of the same color are left on the board,
+ the same position occurred three times. -}
isGameFinished game =
  List.all isOtherKingUnderCheck (nextGames game) ||
  Util.elem (List.map showFigure (Dict.values (gameBoard game)))
       [ List.sort [ "k", "K" ],
         List.sort [ "k", "K" , "b" ],
         List.sort [ "k", "K" , "B" ],
         List.sort [ "k", "K" , "n" ],
         List.sort [ "k", "K" , "N" ],
         List.sort [ "k", "K" , "n" , "n" ],
         List.sort [ "k", "K" , "N" , "N" ]] -- ||
--  (not << List.isEmpty << List.filter (\g -> List.length g >= 3) << Util.groupElements << List.sort << map (showBoard << gameBoard) <| game :: gameHist game)

{-| Returns 'Just' containing the color of the game winner or 'Nothing' if there is no winner. -}
winner game =
  if (isGameFinished game && isKingUnderCheck game)
  then Just << other << gameColor <| game
  else Nothing

{-| Returns a new game state after moving a figure. If the given
move is not possible, it returns Nothing. -}
makeMove : Field -> Field -> Maybe Figure -> Game -> Maybe Game
makeMove from to promotion game =
  let isMatching g =
        case g of
          OngoingGame _ _ _ move ->
            case move of
              RegularMove f t -> f == from && t == to && Util.isNothing promotion
              PromotionMove f t fig -> f == from && t == to && promotion == Just fig
              EnPassantMove f t _ -> f == from && t == to && Util.isNothing promotion
              CastlingMove f t _ _ ->  f == from && t == to && Util.isNothing promotion
          GameStart -> False
  in
    find (always True) <| List.filter isMatching <| validGames game

--{-
--
--elm -m Chess\Game.elm
--elm-repl
--import Chess.Color (..)
--import Chess.Board (..)
--import Chess.Move (..)
--import Chess.Field (..)
--import Chess.Figure (..)
--import Chess.FigureMoves (..)
--import Chess.Game (..)
--import Chess.Util (..)
--gameMessage GameStart
--showGame GameStart
--isOtherKingUnderCheck GameStart -- False
--isKingUnderCheck GameStart -- False
--isGameFinished GameStart -- False
--winner GameStart -- Nothing
--length (nextGames GameStart) -- 20
--length (validGames GameStart) -- 20
--makeMove (field 1 2) (field 1 5) Nothing GameStart -- Nothing
--g1 = makeMove (field 7 2) (field 7 4) Nothing GameStart
--mapMaybe gameMessage g1
--mapMaybe showGame g1
--g2 = maybe Nothing (\g ->  makeMove (field 5 7) (field 5 6) Nothing g) g1
--mapMaybe showGame g2
--g3 = maybe Nothing (\g ->  makeMove (field 6 2) (field 6 4) Nothing g) g2
--mapMaybe showGame g3
--g4 = maybe Nothing (\g ->  makeMove (field 4 8) (field 8 4) Nothing g) g3
--mapMaybe showGame g4
--mapMaybe isOtherKingUnderCheck g4 -- Just False
--mapMaybe isKingUnderCheck g4 -- Just True
--mapMaybe isGameFinished g4 -- Just True
--mapMaybe winner g4 -- Just (Just Black)
--mapMaybe length (mapMaybe nextGames g4) -- Just 20
--mapMaybe length (mapMaybe validGames g4) -- Just 0
--mapMaybe gameMoves g4
--
--import Chess.Util (..)
--import Chess.Field (..)
--import Chess.Game (..)
--g1 = maybe GameStart id <| makeMove (readField "g1") (readField "f3") Nothing GameStart
--g2 = maybe GameStart id <| makeMove (readField "a7") (readField "a6") Nothing g1
--g3 = maybe GameStart id <| makeMove (readField "g2") (readField "g3") Nothing g2
--g4 = maybe GameStart id <| makeMove (readField "a6") (readField "a5") Nothing g3
--g5 = maybe GameStart id <| makeMove (readField "f1") (readField "g2") Nothing g4
--g6 = maybe GameStart id <| makeMove (readField "a5") (readField "a4") Nothing g5
--showGame g6
--map showGame (nextGames g6)
--g7 = maybe GameStart id <| makeMove (readField "e1") (readField "g1") Nothing g6
--
--import Chess.Util (..)
--import Chess.Field (..)
--import Chess.Game (..)
--g1 = maybe GameStart id <| makeMove (readField "e2") (readField "e4") Nothing GameStart
--g2 = maybe GameStart id <| makeMove (readField "a7") (readField "a6") Nothing g1
--g3 = maybe GameStart id <| makeMove (readField "e4") (readField "e5") Nothing g2
--g4 = maybe GameStart id <| makeMove (readField "d7") (readField "d5") Nothing g3
--showGame g4
--map showGame (nextGames g4)
--g5 = maybe GameStart id <| makeMove (readField "e5") (readField "d6") Nothing g4
--showGame g5
--
--:exit
--
---}
