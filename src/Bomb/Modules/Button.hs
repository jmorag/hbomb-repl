---------------------------------------------------
-- Module : Bomb.Modules.Button
---------------------------------------------------
module Bomb.Modules.Button (readButton, button) where

import Bomb
import RIO.Char (toLower)

data ButtonText = Abort | Detonate | Hold | Press
  deriving (Eq)

data ButtonColor = Blue | White | Yellow | Red
  deriving (Eq)

type Button = (ButtonText, ButtonColor)

readButton :: [String] -> Maybe Button
readButton [text, color] =
  liftA2
    (,)
    case map toLower text of
      "abort" -> Just Abort
      "a" -> Just Abort
      "detonate" -> Just Detonate
      "d" -> Just Detonate
      "hold" -> Just Hold
      "h" -> Just Hold
      "press" -> Just Press
      "p" -> Just Press
      _ -> Nothing
    case map toLower color of
      "blue" -> Just Blue
      "b" -> Just Blue
      "white" -> Just White
      "w" -> Just White
      "yellow" -> Just Yellow
      "y" -> Just Yellow
      "red" -> Just Red
      "r" -> Just Red
      _ -> Nothing
readButton _ = Nothing

button :: Button -> Bomb ()
button b = case b of
  (Abort, Blue) -> hold
  (Detonate, _) -> withBatteries \case
    Two -> press
    MoreThanTwo -> press
    LessThanTwo -> case b of
      (_, White) -> withCAR \case
        True -> hold
        False -> withBatteries \case
          MoreThanTwo -> withFRK \case
            True -> press
            _ -> step5
          _ -> step5
      _ -> step5
  _ -> hold
  where
    step5 = case b of
      (_, Yellow) -> hold
      (Hold, Red) -> press
      _ -> hold

press :: Bomb ()
press = output "Press and immediately release the button"

hold :: Bomb ()
hold = do
  output "Hold the button"
  output "Blue strip: release when the countdown timer as a 4 in any position"
  output "Yellow strip: release when the countdown timer as a 5 in any position"
  output "Any other color strip: release when the countdown timer as a 1 in any position"
