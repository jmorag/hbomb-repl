---------------------------------------------------
-- Module : Bomb
--
-- Contains the core bomb types
-- A bomb prelude, if you will
---------------------------------------------------
module Bomb
  ( BombState (..),
    Bomb,
    initialState,
    getLine,
    getChar,
    askYesNo,
    askSerialOdd,
    Batteries (..),
    Indicator (..),
    bat,
    output,
    module X,
    runBomb,
  )
where

import Control.Monad.State.Strict as X
import RIO as X
import RIO.Char
import System.Console.Haskeline

data BombState = BombState
  { batteries :: !(Maybe Batteries),
    serialEven :: !(Maybe Bool),
    serialVowel :: !(Maybe Bool),
    parallel :: !(Maybe Bool),
    strikes :: !Int,
    indicator :: !(Set Indicator)
  }
  deriving (Show)

newtype Bomb a = Bomb {unBomb :: InputT (StateT BombState IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadException)

instance MonadState BombState Bomb where
  state = Bomb . lift . state

runBomb :: Bomb () -> BombState -> IO ()
runBomb bomb st = evalStateT (runInputT defaultSettings (unBomb bomb)) st

output :: String -> Bomb ()
output = Bomb . outputStrLn

getChar :: String -> Bomb (Maybe Char)
getChar = Bomb . getInputChar

getLine :: String -> Bomb (Maybe String)
getLine = Bomb . getInputLine

initialState :: BombState
initialState = BombState Nothing Nothing Nothing Nothing 0 mempty

data Batteries = LessThanTwo | Two | MoreThanTwo deriving (Show)

data Indicator = FRK | CAR deriving (Show, Eq, Ord)

bat :: String -> Bomb ()
bat n = do
  batteries <- case n of
    "gt2" -> pure (Just MoreThanTwo)
    "2" -> pure (Just Two)
    "0" -> pure (Just LessThanTwo)
    "1" -> pure (Just LessThanTwo)
    _ -> case readMaybe @Int n of
      Just n'
        | n' < 0 -> Nothing <$ output "Can't have negative batteries"
        | n' > 2 -> pure (Just MoreThanTwo)
        | otherwise -> error "unreachable"
      Nothing -> Nothing <$ output "Couldn't decipher number of batteries"
  case batteries of
    Just b -> do
      modify' \s -> s {batteries}
      output ("There are " <> show b <> " batteries")
    Nothing -> pure ()

askYesNo :: String -> (Lens' BombState (Maybe Bool)) -> Bomb () -> Bomb ()
askYesNo message boolLens continue = do
  yesNo <- getChar $ message <> " [y/n]\n"
  case fmap toLower yesNo of
    Nothing -> pure ()
    Just 'y' -> do
      modify' (set boolLens (Just True))
      continue
    Just 'n' -> do
      modify' (set boolLens (Just False))
      continue
    Just _ -> do
      output "Please enter 'y' or 'n'"
      askYesNo message boolLens continue

askSerialOdd :: Bomb () -> Bomb ()
askSerialOdd =
  askYesNo
    "Is the last digit of the serial number odd?"
    (lens (fmap not . serialEven) (\st b -> st {serialEven = fmap not b}))
