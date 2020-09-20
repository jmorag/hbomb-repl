module Main where

import Bomb
import Bomb.Modules.Mazes
import Bomb.Modules.SimpleWires
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
    Just input -> case words input of
      ["frk"] -> do
        modify' \s -> s {frk = Just True}
        output "There is a lit indicator FRK"
        loop
      ["car"] -> do
        modify' \s -> s {car = Just True}
        output "There is a lit indicator CAR"
        loop
      ["batteries", n] -> readBatteries n >> loop
      ["bat", n] -> readBatteries n >> loop
      ["even"] -> do
        modify' \s -> s {serialEven = Just True}
        output "Serial number is even"
        loop
      ["odd"] -> do
        modify' \s -> s {serialEven = Just False}
        output "Serial number is odd"
        loop
      ["vowel"] -> do
        modify' \s -> s {serialVowel = Just True}
        output "Serial number has a vowel"
        loop
      ["no", "vowel"] -> do
        modify' \s -> s {serialVowel = Just False}
        output "Serial number does NOT have a vowel"
        loop
      ["parallel"] -> do
        modify' \s -> s {parallel = Just True}
        output "Bomb has a parallel port"
        loop
      ["no", "parallel"] -> do
        modify' \s -> s {parallel = Just False}
        output "Bomb does NOT have a parallel port"
        loop
      ["par"] -> do
        modify' \s -> s {parallel = Just True}
        output "Bomb has a parallel port"
        loop
      ["no", "par"] -> do
        modify' \s -> s {parallel = Just False}
        output "Bomb does NOT have a parallel port"
        loop
      ["strike"] -> do
        modify' \s -> s {strikes = (strikes s) + 1}
        curStrikes <- gets strikes
        output ((show curStrikes) ++ " strikes")
        loop
      ["wires", ws] -> case readSimpleWires ws of
        Just wires -> simpleWires wires >> loop
        Nothing -> output "Simple wires must be one of ['r' (red), 'b' (blue), 'k' (black), 'w' (white), 'y' (yellow)]" >> loop
      "maze" : ixs -> case maze ixs of
        Right path -> output (show path) >> loop
        Left e -> output e >> loop
      _ -> output ("unknown command \"" <> input <> "\"") >> loop
