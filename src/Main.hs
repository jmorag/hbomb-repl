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
      _ -> outputStrLn ("unknown command \"" <> input <> "\"") >> loop

bat :: String -> InputT (StateT BombState IO) ()
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

initialState :: BombState
initialState = BombState Nothing Nothing Nothing Nothing 0 mempty

data Batteries = LessThanTwo | Two | MoreThanTwo deriving (Show)

data Indicator = FRK | CAR deriving (Show, Eq, Ord)
