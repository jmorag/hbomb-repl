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
  [_, _, _, w4] | n Red > 1 -> withSerialOdd \case True -> cut "last"; False -> k4 w4
  [_, _, _, w4] -> k4 w4
  [_, _, _, _, Black] -> withSerialOdd \case True -> cut "fourth"; False -> k5
  [_, _, _, _, _] -> k5
  [_, _, _, _, _, _] | n Yellow == 0 -> withSerialOdd \case True -> cut "third"; False -> k6
  [_, _, _, _, _, _] -> k6
  _ -> output "Bomb must have 3, 4, 5, or 6 wires"
  where
    n color = length (filter (== color) wires)
    cut nth = output ("Cut the " <> nth <> " wire")
    k4 w4
      | w4 == Yellow && n Red == 0 = cut "first"
      | n Blue == 1 = cut "first"
      | n Yellow > 1 = cut "last"
      | otherwise = cut "second"
    k5
      | n Red == 1 && n Yellow > 1 = cut "second"
      | n Black == 0 = cut "second"
      | otherwise = cut "first"
    k6
      | n Yellow == 1 && n White > 1 = cut "fourth"
      | n Red == 0 = cut "last"
      | otherwise = cut "fourth"

readSimpleWires :: String -> Maybe [SimpleWire]
readSimpleWires = traverse \case
  'b' -> Just Blue
  'k' -> Just Black
  'w' -> Just White
  'y' -> Just Yellow
  'r' -> Just Red
  _ -> Nothing
