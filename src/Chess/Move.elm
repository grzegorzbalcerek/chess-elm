module Chess.Move exposing (..)

import Chess.Field as Field exposing (..)
import Chess.Figure as Figure exposing (..)
import String exposing (concat)

type alias From = Field
type alias To = Field
type alias Captured = Field
type alias RookFrom = Field
type alias RookTo = Field

type Move = RegularMove From To
          | PromotionMove From To Figure
          | EnPassantMove From To Captured
          | CastlingMove From To RookFrom RookTo

from move = case move of
  RegularMove field _ -> field
  PromotionMove field _ _ -> field
  EnPassantMove field _ _ -> field
  CastlingMove field _ _ _ -> field

to move = case move of
  RegularMove _ field -> field
  PromotionMove _ field _ -> field
  EnPassantMove _ field _ -> field
  CastlingMove _ field _ _ -> field

{-

elm -m Chess\Move.elm
elm-repl
import Chess.Move (..)
import Chess.Field (..)
import Chess.Figure (..)
showMove (RegularMove (field 2 2) (field 3 3))
:exit

-}
