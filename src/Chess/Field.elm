module Chess.Field exposing (..)

import Char exposing (fromCode,toCode,toLower)
import String exposing (fromChar, cons,toList)
import Chess.Color exposing (Color,firstRow,other)

{-| The game of chess is played on a board with 64 fields.
The board has the shape of a square with eight rows — from 1 to 8
— and eight columns — from `a` to `h`.
The 'Field' represents a board field.
It has members representing the column and the row —
the field coordinates on the chess board.
Valid fields have coordinates in the range between 1 and 8.
-}
type alias Field = { col : Int, row : Int }

{-| Construct a field -}
field : Int -> Int -> Field
field c r = { col = c, row = r }

showRow : Int -> String
showRow row = fromChar (fromCode (48+row))

{-| Shows field coordinates as a pair of characters:
a letter representing the column and a number representing the row.
-}
showField : Field -> String
showField {col,row} = cons (toLower (fromCode (64+col))) (showRow row)

readField : String -> Maybe Field
readField f =
    case (toList f) of
        c :: r :: [] ->
            let col = toCode (Char.toUpper c) - 64
                row = toCode r - 48
            in Just (field col row)
        _ -> Nothing

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

