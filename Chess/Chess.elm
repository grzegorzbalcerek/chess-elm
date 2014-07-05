module Chess.Chess where

import Chess.Util (..)
import Chess.Color (Color,White)
import Chess.Board (Board)
import Chess.Game (Game,GameStart,gameBoard,gameColor,makeMove,gameMessage,isGameFinished,winner,validPromotionsMoves,showGameHist)
import Chess.Field (Field,readField,fieldCol,field,isValid,showField)
import Chess.Figure (Figure,showFigureUnicode,figure)
import Chess.ComputerPlayer (generateMove)
import String (cons,show,append)
import Char (fromCode,toLower)
import Dict
import Mouse
import Time
import Graphics.Input (Input,input,clickable)

-- board drawing

boardSize = 450

fieldSize : Float
fieldSize = boardSize / 9

fieldColor col row =
  if (col+row) `mod` 2 == 1 then yellow else red

fieldForms = [1..8] >>= \col ->
             [1..8] >>= \row ->
             [ move (col*fieldSize - 4.5*fieldSize,row*fieldSize - 4.5*fieldSize) <| filled (fieldColor (round col) (round row)) (rect fieldSize fieldSize) ]

borderForm = [ rect (fieldSize*8) (fieldSize*8) |>
               outlined { defaultLine | width <- 2 } ]

coordinateForms : [Form]
coordinateForms = [1..8] >>= \n ->
                  let rowSymbol = show n |> toText |> Text.height (0.35*fieldSize) |> centered |> toForm
                      colSymbol = cons ((toLower . fromCode) (64+(round n))) "" |> toText |> Text.height (0.35*fieldSize) |> centered |> toForm
                  in [ rowSymbol |> move (-4.25*fieldSize,n*fieldSize-4.5*fieldSize)
                     , rowSymbol |> move (4.25*fieldSize,n*fieldSize-4.5*fieldSize)
                     , colSymbol |> move (n*fieldSize-4.5*fieldSize,4.25*fieldSize)
                     , colSymbol |> move (n*fieldSize-4.5*fieldSize,-4.25*fieldSize) ]

figureForms : Board -> [Form]
figureForms board =
  Dict.toList board >>= \(fieldStr,figure) ->
  let {col,row} = readField fieldStr
      figureString = showFigureUnicode figure
  in [ figureString |>
       toText |>
       Text.height (0.9*fieldSize) |>
       centered |>
       toForm |> 
       move ((toFloat col)*fieldSize-4.5*fieldSize,(toFloat row)*fieldSize-4.5*fieldSize) ]

markerForm : Maybe Field -> [Form]
markerForm maybeField = case maybeField of
  Nothing -> []
  Just {col,row} ->
    [ rect fieldSize fieldSize |>
      outlined { defaultLine | width <- 3, color <- blue } |>
      move ((toFloat col)*fieldSize-4.5*fieldSize,(toFloat row)*fieldSize-4.5*fieldSize) ]

drawBoard game selection =
  collage boardSize boardSize ( fieldForms ++
                                borderForm ++
                                coordinateForms ++
                                (figureForms <| gameBoard game) ++
                                (markerForm selection) )

-- raw signals

mouseClicksPositionSignal = sampleOn Mouse.clicks Mouse.position
timeSignal = Time.fps (2 * Time.second)

data Event = Ignore | ClickedField Field | Tick | SelectedFigure Figure | NewGame Phase

-- new game buttons

newGame : Input Event
newGame = input Ignore

newGameButton : String -> Phase -> Element
newGameButton label phase =
  collage 320 45 [ rect 310 40 |> filled grey
                 , rect 310 40 |> outlined { defaultLine | width <- 2 }
                 , label |> toText |> Text.height (0.4*fieldSize) |> centered |> toForm ] |>
  clickable newGame.handle (NewGame phase)

-- promotion figure buttons

selectedFigure : Input Event
selectedFigure = input Ignore

promotionFigureButton : Figure -> Element
promotionFigureButton figure =
  collage (round <| fieldSize + 5) (round <| fieldSize + 5) [ rect fieldSize fieldSize |> outlined { defaultLine | width <- 2 }
                , figure |>
                  showFigureUnicode |>
                  toText |>
                  Text.height (0.9*fieldSize) |>
                  centered |>
                  toForm ] |>
  clickable selectedFigure.handle (SelectedFigure figure)
       
-- signals

clickedFieldSignal =
  let transform mousePosition =
          let (x,y) = mousePosition
              col = round <| (toFloat x) / fieldSize
              row = 9 - (round <| (toFloat y) / fieldSize)
              f = field col row
          in
              if isValid f
              then ClickedField f
              else Ignore
  in lift transform mouseClicksPositionSignal

ticksSignal = lift (always Tick) timeSignal

-- merged signal

eventsSignal = newGame.signal `merge` selectedFigure.signal `merge` clickedFieldSignal `merge` ticksSignal

-- game state signal

data Phase = GameFinished | PlayerMove | PlayerMoved | ComputerMove | PromotionFigureChoice Field [Figure]

type State = { phase: Phase, game: Game, message: Maybe String, selection: Maybe Field, event: Maybe Event }

initialState = { phase = PlayerMove, game = GameStart, message = Nothing, selection = Nothing, event = Nothing }

handleMakeMove state makeMoveResult = case makeMoveResult of
  Nothing -> { state | selection <- Nothing
                     , message <- Just "Wrong Move" }
  Just nextGame -> { state | game <- nextGame
                           , message <- Nothing
                           , selection <- Nothing
                           , phase <- if isGameFinished nextGame
                                      then GameFinished
                                      else PlayerMoved }

handleGenerateMove state generateMoveResult = case generateMoveResult of
        Nothing       -> { state | message <- Nothing }
        Just nextGame -> { state | game <- nextGame
                                 , message <- Nothing
                                 , selection <- Nothing
                                 , phase <- if isGameFinished nextGame
                                            then GameFinished
                                            else PlayerMove }

step signalValue state =
  let {phase,game,message,selection} = state
  in case (phase, selection, signalValue) of
    (_,_,NewGame startPhase)                                  -> { initialState | phase <- startPhase }
    (GameFinished,_,_)                                        -> state
    (_,_,Ignore)                                             -> { state | message <- Nothing, selection <- Nothing }
    (PlayerMoved,_,Tick)                                    -> { state | phase <- ComputerMove }
    (ComputerMove,_,Tick)                                   -> handleGenerateMove state <| generateMove game
    (PlayerMove,Nothing,ClickedField clickedField)            -> { state | selection <- Just clickedField }
    (PlayerMove,Just selectedField,ClickedField clickedField) ->
      case (validPromotionsMoves game selectedField clickedField) of
        []        -> handleMakeMove state <| makeMove selectedField clickedField Nothing game
        fig :: [] -> handleMakeMove state <| makeMove selectedField clickedField (Just fig) game
        figures   -> { state | message <- Just "Choose the promotion figure:"
                             , phase <- PromotionFigureChoice clickedField figures }
    (PromotionFigureChoice clickedField _,Just selectedField,SelectedFigure fig) ->
      handleMakeMove state <| makeMove selectedField clickedField (Just fig) game
    _                                              -> state

gameStateSignal = foldp step initialState eventsSignal

-- view

halfFieldSpacer = spacer (round (fieldSize / 2)) (round (fieldSize / 2))

chooseMessage state =
  if state.phase == GameFinished
  then "Game Over. " `append` (case winner state.game of
                                                Just color -> "Winner: " `append` show color `append` "."
                                                Nothing -> "Draw.")
  else maybe (gameMessage state.game) id state.message

view state =
  let {game,message,selection,phase} = state
  in
    flow right [
      flow down
      [ drawBoard game selection
      , flow right [ halfFieldSpacer, chooseMessage state |> toText |> Text.height (0.5*fieldSize) |> centered ]
      , flow right [ halfFieldSpacer,
        case phase of
          PromotionFigureChoice _ figures -> flow right <| map promotionFigureButton figures
          _ -> empty ]
      ],
      flow down
      [ halfFieldSpacer
      , newGameButton "Start a new game, you begin" PlayerMove
      , newGameButton "Start a new game, computer begins" ComputerMove
      , halfFieldSpacer
      , flow right [ halfFieldSpacer, plainText "To move a figure, click on it" ]
      , flow right [ halfFieldSpacer, plainText "and then click on the destination field." ]
      , halfFieldSpacer
      , showGameHist game |> toText |> monospace |> Text.height (0.3*fieldSize) |> leftAligned
      ]
    ]

-- main

main = view <~ gameStateSignal
