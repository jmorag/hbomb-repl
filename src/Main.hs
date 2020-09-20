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
      ["batteries", n] -> bat n >> loop
      ["bat", n] -> bat n >> loop
      ["wires", ws] -> case readSimpleWires ws of
        Just wires -> simpleWires wires >> loop
        Nothing -> output "Simple wires must be one of ['r' (red), 'b' (blue), 'k' (black), 'w' (white), 'y' (yellow)]" >> loop
      _ -> output ("unknown command \"" <> input <> "\"") >> loop
