module Chess.Util exposing (..)

takeWhile predicate lst = case lst of
  [] -> []
  (x::xs) -> if (predicate x)
             then x :: takeWhile predicate xs
             else []

dropWhile predicate lst = case lst of
  [] -> []
  (x::xs) -> if (predicate x)
             then dropWhile predicate xs
             else lst

find : (a -> Bool) -> List a -> Maybe a
find f lst = case (List.filter f lst) of
  [] -> Nothing
  x::_ -> Just x

elem : a -> List a -> Bool
elem e lst = not << List.isEmpty <| List.filter (\x -> x == e) lst

isNothing maybe =
    case maybe of
        Nothing -> True
        Just _ -> False

--groupElementsOnSortedList lst =
--  let group1 = List.foldl (\x ggs -> case ggs of
--                                       g :: gs -> if (x == List.head g) then (x::g)::gs else [x]::g::gs
--                                       [] -> [] )
--  in case (List.reverse lst) of
--       [] -> []
--       x::xs -> group1 [[x]] xs

{--

elm -m Chess\Util.elm
elm-repl
import Chess.Util (..)
takeWhile (\x -> x < 5) [2,4,1,7,2]
dropWhile (\x -> x < 5) [2,4,1,7,2]
mapMaybe (\x -> x + 5) <| Just 4
elem 2 [1,2,3]
groupElements [1,2,2,3,5,5,3,2]
:exit

-}
