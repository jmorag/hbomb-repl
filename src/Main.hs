module Main where

import Bomb
import Bomb.Modules.SimpleWires
import RIO.Char
import qualified RIO.Set as Set

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
        modify' \s -> s {indicator = Set.insert FRK (indicator s)}
        output "There is a lit indicator FRK"
        loop
      ["car"] -> do
        modify' \s -> s {indicator = Set.insert CAR (indicator s)}
        output "There is a lit indicator CAR"
        loop
      ["batteries", n] -> readBatteries n >> loop
      ["bat", n] -> readBatteries n >> loop
      ["even"] -> do
        lift $ modify' \s -> s {serialEven = Just True}
        outputStrLn "Serial number is even"
        loop
      ["odd"] -> do
        lift $ modify' \s -> s {serialEven = Just False}
        outputStrLn "Serial number is odd"
        loop
      ["vowel"] -> do
        lift $ modify' \s -> s {serialVowel = Just True}
        outputStrLn "Serial number has a vowel"
        loop
      ["no", "vowel"] -> do
        lift $ modify' \s -> s {serialVowel = Just False}
        outputStrLn "Serial number does NOT have a vowel"
        loop
      ["parallel"] -> do
        lift $ modify' \s -> s {parallel = Just True}
        outputStrLn "Bomb has a parallel port"
        loop
      ["no", "parallel"] -> do
        lift $ modify' \s -> s {parallel = Just False}
        outputStrLn "Bomb does NOT have a parallel port"
        loop
      ["strike"] -> do
        lift $ modify' \s -> s {strikes = (strikes s) + 1}
        curStrikes <- lift $ gets strikes
        outputStrLn ((show curStrikes) ++ " strikes")
        loop
      ["wires", ws] -> case readSimpleWires ws of
        Just wires -> simpleWires wires >> loop
        Nothing -> output "Simple wires must be one of ['r' (red), 'b' (blue), 'k' (black), 'w' (white), 'y' (yellow)]" >> loop
      _ -> output ("unknown command \"" <> input <> "\"") >> loop
