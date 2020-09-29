module Main where

import Bomb
import Bomb.Modules.Button
import Bomb.Modules.ComplicatedWires
import Bomb.Modules.Knobs
import Bomb.Modules.Mazes
import Bomb.Modules.Memory
import Bomb.Modules.MorseCode
import Bomb.Modules.Passwords
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
        ["reset"] -> put initialState >> output "RESET"
        ["dump"] -> gets show >>= output
        ["frk"] -> do
          modify' \s -> s {frk = Just True}
          output "There is a lit indicator FRK"
        ["car"] -> do
          modify' \s -> s {car = Just True}
          output "There is a lit indicator CAR"
        ["nofrk"] -> do
          modify' \s -> s {frk = Just False}
          output "There is NO lit indicator FRK"
        ["nocar"] -> do
          modify' \s -> s {car = Just False}
          output "There is NO lit indicator CAR"
        ["batteries", n] -> readBatteries n
        ["bat", n] -> readBatteries n
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
        ["mem", "reset"] -> do
          output "Reset Memory"
          modify' \s -> s {memory = mempty}
        ["memory", "reset"] -> do
          output "Reset Memory"
          modify' \s -> s {memory = mempty}
        ["mem", n] -> mem n
        ["memory", n] -> mem n
        ["strike"] -> do
          modify' \s -> s {strikes = (strikes s) + 1}
          curStrikes <- gets strikes
          output ((show curStrikes) ++ " strike" ++ if curStrikes == 1 then "" else "s")
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
        "pass" : i -> passwords i
        "comp" : w -> complicated w
        "morse" : m -> morse m
        "omorse" : m -> omorse m
        _ -> output ("unknown command \"" <> input <> "\"")
      loop

readBatteries :: String -> Bomb ()
readBatteries = \case
  "gt2" -> modify' \s -> s {batteries = Just MoreThanTwo}
  ">2" -> modify' \s -> s {batteries = Just MoreThanTwo}
  "2" -> modify' \s -> s {batteries = Just Two}
  "0" -> modify' \s -> s {batteries = Just LessThanTwo}
  "1" -> modify' \s -> s {batteries = Just LessThanTwo}
  n -> case readMaybe @Int n of
    Just n'
      | n' < 0 -> output "Can't have negative batteries"
      | n' > 2 -> modify' \s -> s {batteries = Just MoreThanTwo}
      | otherwise -> error "unreachable"
    Nothing -> output "Please enter a number"
