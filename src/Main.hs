module Main where

import Control.Monad.Trans.State.Strict
import RIO
import RIO.Char
import qualified RIO.Set as Set
import System.Console.Haskeline

main :: IO ()
main = evalStateT (runInputT defaultSettings loop) initialState

loop :: InputT (StateT BombState IO) ()
loop = do
  minput <- getInputLine "hbomb> "
  case map toLower <$> minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "" -> loop
    Just input -> case words input of
      ["frk"] -> do
        lift $ modify' \s -> s {indicator = Set.insert FRK (indicator s)}
        outputStrLn "There is a lit indicator FRK"
        loop
      ["car"] -> do
        lift $ modify' \s -> s {indicator = Set.insert CAR (indicator s)}
        outputStrLn "There is a lit indicator CAR"
        loop
      ["batteries", n] -> bat n
      ["bat", n] -> bat n
      ["wires", ws] -> case readSimpleWires ws of
        Just wires -> simpleWires wires >> loop
        Nothing -> outputStrLn "Simple wires must be one of ['r' (red), 'b' (blue), 'k' (black), 'w' (white), 'y' (yellow)]" >> loop
      _ -> outputStrLn ("unknown command \"" <> input <> "\"") >> loop

bat :: String -> Bomb ()
bat n = do
  batteries <- case n of
    "gt2" -> pure (Just MoreThanTwo)
    "2" -> pure (Just Two)
    "0" -> pure (Just LessThanTwo)
    "1" -> pure (Just LessThanTwo)
    _ -> case readMaybe @Int n of
      Just n'
        | n' < 0 -> Nothing <$ outputStrLn "Can't have negative batteries"
        | n' > 2 -> pure (Just MoreThanTwo)
        | otherwise -> error "unreachable"
      Nothing -> Nothing <$ outputStrLn "Couldn't decipher number of batteries"
  case batteries of
    Just b -> do
      lift $ modify' \s -> s {batteries}
      outputStrLn ("There are " <> show b <> " batteries")
    Nothing -> outputStrLn ("There is an unknown number of batteries")
  loop

data BombState = BombState
  { batteries :: !(Maybe Batteries),
    serialEven :: !(Maybe Bool),
    serialVowel :: !(Maybe Bool),
    parallel :: !(Maybe Bool),
    strikes :: !Int,
    indicator :: !(Set Indicator)
  }
  deriving (Show)

type Bomb = InputT (StateT BombState IO)

initialState :: BombState
initialState = BombState Nothing Nothing Nothing Nothing 0 mempty

data Batteries = LessThanTwo | Two | MoreThanTwo deriving (Show)

data Indicator = FRK | CAR deriving (Show, Eq, Ord)

data SimpleWire = Red | Black | White | Yellow | Blue
  deriving (Show, Eq)

simpleWires :: [SimpleWire] -> Bomb ()
simpleWires wires = do
  serialOdd <- (fmap not) <$> lift (gets serialEven)
  case wires of
    [_, _, w3] ->
      if  | n Red == 0 -> cut "second"
          | w3 == White -> cut "last"
          | n Blue > 1 -> cut "last blue"
          | otherwise -> cut "last"
    [_, _, _, w4] ->
      if  | n Red > 1 && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Red > 1 && serialOdd == Just True -> cut "last"
          | w4 == Yellow && n Red == 0 -> cut "first"
          | n Blue == 1 -> cut "first"
          | n Yellow > 1 -> cut "last"
          | otherwise -> cut "second"
    [_, _, _, _, w5] ->
      if  | w5 == Black && serialOdd == Just True -> cut "fourth"
          | w5 == Black && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Red == 1 && n Yellow > 1 -> cut "second"
          | n Black == 0 -> cut "second"
          | otherwise -> cut "first"
    [_, _, _, _, _, _] ->
      if  | n Yellow == 0 && serialOdd == Just True -> cut "third"
          | n Yellow == 0 && serialOdd == Nothing -> askSerialOdd (simpleWires wires)
          | n Yellow == 1 && n White > 1 -> cut "fourth"
          | n Red == 0 -> cut "last"
          | otherwise -> cut "fourth"
  where
    n color = length (filter (== color) wires)
    cut nth = outputStrLn ("Cut the " <> nth <> " wire")

readSimpleWires :: String -> Maybe [SimpleWire]
readSimpleWires = traverse \case
  'b' -> Just Blue
  'k' -> Just Black
  'w' -> Just White
  'y' -> Just Yellow
  'r' -> Just Red
  _ -> Nothing

askYesNo :: String -> (Lens' BombState (Maybe Bool)) -> Bomb () -> Bomb ()
askYesNo message boolLens continue = do
  yesNo <- getInputChar $ message <> " [y/n]\n"
  case fmap toLower yesNo of
    Nothing -> pure ()
    Just 'y' -> do
      lift $ modify' (set boolLens (Just True))
      continue
    Just 'n' -> do
      lift $ modify' (set boolLens (Just False))
      continue
    Just _ -> do
      outputStrLn "Please enter 'y' or 'n'"
      askYesNo message boolLens continue

askSerialOdd :: Bomb () -> Bomb ()
askSerialOdd =
  askYesNo
    "Is the last digit of the serial number odd?"
    (lens (fmap not . serialEven) (\st b -> st {serialEven = fmap not b}))
