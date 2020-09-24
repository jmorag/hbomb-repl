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
readWire s =
  if (any (`Set.notMember` (Set.fromList ["red", "r", "blue", "b", "star", "s", "led", "l"])) (map (map toLower) s))
    then Nothing
    else
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
instructionToAction = \case
  Cut -> cut
  DoNotCut -> dont
  ParallelPort -> withParallel cutIf
  SerialEven -> withSerialEven cutIf
  TwoOrMoreBatteries -> withBatteries \case
    Two -> cut
    MoreThanTwo -> cut
    LessThanTwo -> dont

cut, dont :: Bomb ()
cut = output "Cut the wire"
dont = output "Do NOT cut the wire"

cutIf :: Bool -> Bomb ()
cutIf b = if b then cut else dont
