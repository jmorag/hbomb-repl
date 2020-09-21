module Bomb.Modules.ComplicatedWires where

import Bomb
import RIO.Char
import qualified RIO.Set as Set

data Wire = Wire
  { red :: Bool,
    blue :: Bool,
    star :: Bool,
    led :: Bool
  }
  deriving (Show)

data Instruction = Cut | DoNotCut | SerialEven | ParallelPort | TwoOrMoreBatteries

complicated :: [String] -> Bomb ()
complicated s =
  case (readWire s) of
    Just w -> instructionToAction $ (wireInstruction w)
    Nothing -> output "Failed to read wire"

readWire :: [String] -> Maybe Wire
readWire s = if (any (`Set.notMember` (Set.fromList ["red", "r", "blue", "b" ,"star", "s", "led", "l"])) (map (map toLower) s)) then Nothing else
  let s' = Set.fromList (map (map toLower) s)
      red = Set.member "red" s' || Set.member "r" s'
      blue = Set.member "blue" s' || Set.member "b" s'
      star = Set.member "star" s' || Set.member "s" s'
      led = Set.member "led" s' || Set.member "l" s'
  in Just Wire {..}

wireInstruction :: Wire -> Instruction
wireInstruction = \case
  Wire True True True True -> DoNotCut
  Wire True True True False -> ParallelPort
  Wire True True False True -> SerialEven
  Wire True True False False -> SerialEven
  Wire True False True True -> TwoOrMoreBatteries
  Wire True False True False -> Cut
  Wire True False False True -> TwoOrMoreBatteries
  Wire True False False False -> SerialEven
  Wire False True True True -> ParallelPort
  Wire False True True False -> DoNotCut
  Wire False True False True -> ParallelPort
  Wire False True False False -> SerialEven
  Wire False False True True -> TwoOrMoreBatteries
  Wire False False True False -> Cut
  Wire False False False True -> DoNotCut
  Wire False False False False -> Cut


instructionToAction :: Instruction -> Bomb ()
instructionToAction i = do
  bats <- gets batteries
  parallelPort <- gets parallel
  serialEven <- gets serialEven
  case i of Cut -> output "Cut the wire"
            DoNotCut -> output "Do NOT cut the wire"
            ParallelPort -> case parallelPort of Nothing -> askParallel (output "Cut the wire") (output "Do NOT cut the wire")
                                                 Just True -> output "Cut the wire"
                                                 Just False -> output "Do NOT cut the wire"
            SerialEven -> case serialEven of Nothing -> askSerialEven (output "Cut the wire") (output "Do NOT cut the wire")
                                             Just True -> output "Cut the wire"
                                             Just False -> output "Do NOT cut the wire"
            TwoOrMoreBatteries -> case bats of Nothing -> askBatteries \case
                                                 MoreThanTwo -> output "Cut the wire"
                                                 Two -> output "Cut the wire"
                                                 _ -> output "Do NOT cut the wire"
                                               Just MoreThanTwo -> output "Cut the wire"
                                               Just Two -> output "Cut the wire"
                                               _ -> output "Do NOT cut the wire"
