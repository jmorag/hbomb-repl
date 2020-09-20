---------------------------------------------------
-- Module : Bomb.Modules.SimpleWires
---------------------------------------------------
module Bomb.Modules.SimpleWires where

import Bomb

data SimpleWire = Red | Black | White | Yellow | Blue
  deriving (Show, Eq)

simpleWires :: [SimpleWire] -> Bomb ()
simpleWires wires = do
  serialOdd <- (fmap not) <$> (gets serialEven)
  case wires of
    [_, _, w3] ->
      if  | n Red == 0 -> cut "second"
          | w3 == White -> cut "last"
          | n Blue > 1 -> cut "last blue"
          | otherwise -> cut "last"
    [_, _, _, w4] ->
      if  | n Red > 1 && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Red > 1 && serialOdd == Just True -> cut "last"
          | w4 == Yellow && n Red == 0 -> cut "first"
          | n Blue == 1 -> cut "first"
          | n Yellow > 1 -> cut "last"
          | otherwise -> cut "second"
    [_, _, _, _, w5] ->
      if  | w5 == Black && serialOdd == Just True -> cut "fourth"
          | w5 == Black && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Red == 1 && n Yellow > 1 -> cut "second"
          | n Black == 0 -> cut "second"
          | otherwise -> cut "first"
    [_, _, _, _, _, _] ->
      if  | n Yellow == 0 && serialOdd == Just True -> cut "third"
          | n Yellow == 0 && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Yellow == 1 && n White > 1 -> cut "fourth"
          | n Red == 0 -> cut "last"
          | otherwise -> cut "fourth"
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
