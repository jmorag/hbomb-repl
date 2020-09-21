module Main where

import Bomb
import Bomb.Modules.Button
import Bomb.Modules.Knobs
import Bomb.Modules.Mazes
import Bomb.Modules.SimpleWires
import Bomb.Modules.WhosOnFirst
import RIO.Char

main :: IO ()
main = runBomb loop initialState

loop :: Bomb ()
loop = do
  minput <- getLine "hbomb> "
  case map toLower <$> minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "" -> loop
    Just input -> do
      case words input of
        ["frk"] -> do
          modify' \s -> s {frk = Just True}
          output "There is a lit indicator FRK"
        ["car"] -> do
          modify' \s -> s {car = Just True}
          output "There is a lit indicator CAR"
        ["batteries", n] -> void $ readBatteries n
        ["bat", n] -> void $ readBatteries n
        "button" : rest -> case readButton rest of
          Just b -> button b
          Nothing -> output "Usage: button <text> <color>"
        ["even"] -> do
          modify' \s -> s {serialEven = Just True}
          output "Serial number is even"
        ["odd"] -> do
          modify' \s -> s {serialEven = Just False}
          output "Serial number is odd"
        ["vowel"] -> do
          modify' \s -> s {serialVowel = Just True}
          output "Serial number has a vowel"
        ["no", "vowel"] -> do
          modify' \s -> s {serialVowel = Just False}
          output "Serial number does NOT have a vowel"
        ["parallel"] -> do
          modify' \s -> s {parallel = Just True}
          output "Bomb has a parallel port"
        ["no", "parallel"] -> do
          modify' \s -> s {parallel = Just False}
          output "Bomb does NOT have a parallel port"
        ["par"] -> do
          modify' \s -> s {parallel = Just True}
          output "Bomb has a parallel port"
        ["no", "par"] -> do
          modify' \s -> s {parallel = Just False}
          output "Bomb does NOT have a parallel port"
        ["strike"] -> do
          modify' \s -> s {strikes = (strikes s) + 1}
          curStrikes <- gets strikes
          output ((show curStrikes) ++ " strikes")
        ["wires", ws] -> case readSimpleWires ws of
          Just wires -> simpleWires wires
          Nothing -> output "Simple wires must be one of ['r' (red), 'b' (blue), 'k' (black), 'w' (white), 'y' (yellow)]"
        "maze" : ixs -> case maze ixs of
          Right path -> output (show path)
          Left e -> output e
        "wof1" : w -> case wofOne (unwords w) of
          Just dir -> output dir
          Nothing -> output "Unknown word"
        "wof2" : w -> case wofTwo (unwords w) of
          Just l -> output (show l)
          Nothing -> output "Unknown word"
        ["knobs", ks] -> knobs ks
        _ -> output ("unknown command \"" <> input <> "\"")
      loop
