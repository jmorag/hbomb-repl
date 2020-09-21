module Bomb.Modules.Knobs (knobs) where

import Bomb

knobs :: String -> Bomb ()
knobs ks = case ks of
  "001011" -> output "Up"
  "011001" -> output "Down"
  "000010" -> output "Left"
  "101111" -> output "Right"
  "101010" -> getBottom
  "101100" -> output "Right"
  _ | length ks == 6 && all (`elem` ['0', '1']) ks -> output "Unknown LED configuration"
  _ -> output "Enter the top row of LEDs: ex. 'knobs 001011'"
  where
    getBottom = do
      bottom <- getChar "What is the third LED on the bottom row? [0/1] (q to abort)"
      case bottom of
        Nothing -> pure ()
        Just 'q' -> pure ()
        Just '1' -> output "Up"
        Just '0' -> output "Down"
        Just _ -> getBottom
