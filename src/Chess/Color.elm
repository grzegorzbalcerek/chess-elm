module Chess.Color exposing (..)

-- | The 'Color' class represents one of the two colors ('Black' or 'White')
-- used in the game of Chess.
type Color = White | Black

showColor color = case color of
  White -> "White"
  Black -> "Black"

-- | The 'other' method returns the opposite color.
other : Color -> Color
other color = case color of
  White -> Black
  Black -> White

-- | The 'firstRow' method returns the coordinate of the first row
-- from the point of view of a player who plays the given color.
firstRow : Color -> Int
firstRow color = case color of
  White -> 1
  Black -> 8
