---------------------------------------------------
-- Module : Bomb.Modules.SimpleWires
---------------------------------------------------
module Bomb.Modules.SimpleWires (simpleWires, readSimpleWires) where

import Bomb

data SimpleWire = Red | Black | White | Yellow | Blue
  deriving (Show, Eq)

simpleWires :: [SimpleWire] -> Bomb ()
simpleWires wires = case wires of
  [_, _, w3]
    | n Red == 0 -> cut "second"
    | w3 == White -> cut "last"
    | n Blue > 1 -> cut "last blue"
    | otherwise -> cut "last"
  [_, _, _, w4] ->
    condM
      [ (just (n Red > 1) &&& askSerialOdd, cut "second"),
        (just (w4 == Yellow), cut "first"),
        (just (n Blue == 1), cut "first"),
        (just (n Yellow > 1), cut "last"),
        (just True, cut "second")
      ]
  [_, _, _, _, w5] ->
    condM
      [ (just (w5 == Black) &&& askSerialOdd, cut "fourth"),
        (just (n Red == 1 && n Yellow > 1), cut "first"),
        (just (n Black == 0), cut "second"),
        (just True, cut "first")
      ]
  [_, _, _, _, _, _] ->
    condM
      [ (just (n Yellow == 0) &&& askSerialOdd, cut "third"),
        (just (n Yellow == 1 && n White > 1), cut "fourth"),
        (just (n Red == 0), cut "last"),
        (just True, cut "fourth")
      ]
  _ -> output "Bomb must have 3, 4, 5, or 6 wires"
  where
    n color = length (filter (== color) wires)
    cut nth = output ("Cut the " <> nth <> " wire")

readSimpleWires :: String -> Maybe [SimpleWire]
readSimpleWires = traverse \case
  'b' -> Just Blue
  'k' -> Just Black
  'w' -> Just White
  'y' -> Just Yellow
  'r' -> Just Red
  _ -> Nothing
