module Chess.Field where

import Char (fromCode,toCode,toLower)
import String (show,cons,toList)
import Chess.Color (firstRow,other)
import Chess.Color
type Color = Chess.Color.Color

{-| The game of chess is played on a board with 64 fields.
The board has the shape of a square with eight rows — from 1 to 8
— and eight columns — from `a` to `h`.
The 'Field' represents a board field.
It has members representing the column and the row —
the field coordinates on the chess board.
Valid fields have coordinates in the range between 1 and 8.
-}
type Field = { col : Int, row : Int }

{-| Construct a field -}
field : Int -> Int -> Field
field c r = { col = c, row = r }

{-| Shows field coordinates as a pair of characters:
a letter representing the column and a number representing the row.
-}
showField : Field -> String
showField {col,row} = (toLower << fromCode) (64+col) `cons` show row

readField : String -> Field
readField f =
  let (c :: r :: []) = toList f
      col = toCode c - 64
      row = toCode r - 48
  in field col row

{-| Returns a new field with coordinates moved
by the given number of rows and columns relative to the original field.
-}
relative : Field -> Int -> Int -> Field
relative {col,row} c r = { col = col+c, row = row+r }

{-| Returns a boolean value indicating
whether the given field belongs to the last row from
the point of view of a player.
-}
isLastRow : Field -> Color -> Bool
isLastRow {row} color = row == firstRow (other color)

{-| Returns a boolean value indicating
whether the field has valid coordinates, that is
whether it belongs to the board.
-}
isValid : Field -> Bool
isValid {col,row} = col >= 1 && col <= 8 && row >= 1 && row <= 8

{-

elm -m Chess\Field.elm
elm-repl
import Chess.Field (..)
import Chess.Color (..)
c4 : Field \
c4 = {col=3,row=4}
d8 : Field \
d8 = {col=4,row=8}
showField c4 -- "C4" : String
readField "c4"
relative c4 1 1 -- { col = 4, row = 5 } : Field
showField <| relative c4 1 1 -- "D5" : String
isLastRow c4 Black -- False : Bool
isLastRow d8 Black -- False : Bool
isLastRow d8 White -- True : Bool
isValid c4 -- True : Bool
:exit

-}
