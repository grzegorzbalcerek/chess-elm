module Chess.Move where

import Chess.Field (..)
import Chess.Figure (..)
import String (concat)

type From = Field
type To = Field
type Captured = Field
type RookFrom = Field
type RookTo = Field

data Move = RegularMove From To
          | PromotionMove From To Figure
          | EnPassantMove From To Captured
          | CastlingMove From To RookFrom RookTo

from move = case move of
  RegularMove from _ -> from
  PromotionMove from _ _ -> from
  EnPassantMove from _ _ -> from
  CastlingMove from _ _ _ -> from

to move = case move of
  RegularMove _ to -> to
  PromotionMove _ to _ -> to
  EnPassantMove _ to _ -> to
  CastlingMove _ to _ _ -> to

{-

elm -m Chess\Move.elm
elm-repl
import Chess.Move (..)
import Chess.Field (..)
import Chess.Figure (..)
showMove (RegularMove (field 2 2) (field 3 3))
:exit

-}
