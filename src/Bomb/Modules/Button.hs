---------------------------------------------------
-- Module : Bomb.Modules.Button
---------------------------------------------------
module Bomb.Modules.Button (readButton, button) where

import Bomb
import RIO.Char

data ButtonText = Abort | Detonate | Hold
  deriving (Eq)

data ButtonColor = Blue | White | Yellow | Red
  deriving (Eq)

type Button = (ButtonText, ButtonColor)

readButton :: [String] -> Maybe Button
readButton [text, color] =
  liftA2
    (,)
    ( case map toLower text of
        "abort" -> Just Abort
        "a" -> Just Abort
        "detonate" -> Just Detonate
        "d" -> Just Detonate
        "hold" -> Just Hold
        "h" -> Just Hold
        _ -> Nothing
    )
    ( case map toLower color of
        "blue" -> Just Blue
        "b" -> Just Blue
        "white" -> Just White
        "w" -> Just White
        "yellow" -> Just Yellow
        "y" -> Just Yellow
        "red" -> Just Red
        "r" -> Just Red
        _ -> Nothing
    )
readButton _ = Nothing

button :: Button -> Bomb ()
button b@(text, color) = do
  bats <- gets batteries
  c <- gets car
  f <- gets frk
  if
      | b == (Abort, Blue) -> hold
      | text == Detonate && bats `elem` [Just Two, Just MoreThanTwo] -> press
      | text == Detonate && bats == Nothing -> askBatteries \case
        LessThanTwo -> button b
        _ -> press
      | color == White && c == Just True -> hold
      | color == White && c == Nothing -> askCAR hold (button b)
      | bats == Just MoreThanTwo && f == Just True -> press
      | bats == Nothing && f == Just True -> askBatteries \case
        MoreThanTwo -> press
        _ -> button b
      | bats == Just MoreThanTwo && f == Nothing -> askFRK press (button b)
      | bats == Nothing && f == Nothing -> askBatteries \case
        MoreThanTwo -> askFRK press (button b)
        _ -> button b
      | color == Yellow -> hold
      | b == (Hold, Red) -> press
      | otherwise -> hold

press :: Bomb ()
press = output "Press and immediately release the button"

hold :: Bomb ()
hold = do
  output "Hold the button"
  output "Blue strip: release when the countdown timer as a 4 in any position"
  output "Yellow strip: release when the countdown timer as a 5 in any position"
  output "Any other color strip: release when the countdown timer as a 1 in any position"
