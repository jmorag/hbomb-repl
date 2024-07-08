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
button b@(text, color) =
  condM
    [ (just (b == (Abort, Blue)), hold),
      (just (text == Detonate) &&& (askBatteries (/= LessThanTwo)), press),
      (just (color == White) &&& askCAR, hold),
      (askBatteries (== MoreThanTwo) &&& askFRK, press),
      (just (color == Yellow), hold),
      (just (b == (Hold, Red)), press),
      (just True, hold)
    ]

press :: Bomb ()
press = output "Press and immediately release the button"

hold :: Bomb ()
hold = do
  output "Hold the button"
  output "Blue strip: release when the countdown timer has a 4 in any position"
  output "Yellow strip: release when the countdown timer has a 5 in any position"
  output "Any other color strip: release when the countdown timer has a 1 in any position"
