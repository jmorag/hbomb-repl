module Bomb.Modules.Memory where

import Bomb
import RIO.Char.Partial
import RIO.Seq (index, (|>))

mem :: String -> Bomb ()
mem disp = do
  stage <- gets ((+ 1) . length . memory)
  output $ "Memory stage: " <> show stage
  case stage of
    1 -> case disp of
      "1" -> pressPos 2
      "2" -> pressPos 2
      "3" -> pressPos 3
      "4" -> pressPos 4
      _ -> output "Display must read 1, 2, 3, or 4"
    2 -> case disp of
      "1" -> pressLabel 4
      "2" -> pressPos =<< getPos 1
      "3" -> pressPos 1
      "4" -> pressPos =<< getPos 1
      _ -> output "Display must read 1, 2, 3, or 4"
    3 -> case disp of
      "1" -> pressLabel =<< getLabel 2
      "2" -> pressLabel =<< getLabel 1
      "3" -> pressPos 3
      "4" -> pressLabel 4
      _ -> output "Display must read 1, 2, 3, or 4"
    4 -> case disp of
      "1" -> pressPos =<< getPos 1
      "2" -> pressPos 1
      "3" -> pressPos =<< getPos 2
      "4" -> pressPos =<< getPos 2
      _ -> output "Display must read 1, 2, 3, or 4"
    5 -> case disp of
      "1" -> pressLabel =<< getLabel 1
      "2" -> pressLabel =<< getLabel 2
      "3" -> pressLabel =<< getLabel 4
      "4" -> pressLabel =<< getLabel 3
      _ -> output "Display must read 1, 2, 3, or 4"
    _ -> error $ "Invalid memory stage " <> show stage

getPos :: Int -> Bomb Position
getPos stage = gets (memory >>> flip index (stage - 1) >>> fst)

getLabel :: Int -> Bomb Label
getLabel stage = gets (memory >>> flip index (stage - 1) >>> snd)

pressPos :: Position -> Bomb ()
pressPos (Position n) = do
  output $
    "Press the button in the " <> case n of
      1 -> "1st"
      2 -> "2nd"
      3 -> "3rd"
      4 -> "4th"
      _ -> error "unreachable"
      <> " position"
  askLabel
  where
    askLabel =
      getChar "What is the label? " >>= \case
        Just l | l `elem` ("1234" :: String) ->
          modify' \s -> s {memory = memory s |> (Position n, Label (digitToInt l))}
        _ -> pure ()

pressLabel :: Label -> Bomb ()
pressLabel (Label l) = do
  output $
    "Press the button labeled " <> case l of
      1 -> "1"
      2 -> "2"
      3 -> "3"
      4 -> "4"
      _ -> error "unreachable"
  askPos
  where
    askPos =
      getChar "What is the position? " >>= \case
        Just p | p `elem` ("1234" :: String) ->
          modify' \s -> s {memory = memory s |> (Position (digitToInt p), Label l)}
        _ -> pure ()
