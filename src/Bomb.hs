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
    askSerialEven,
    askSerialVowel,
    askParallel,
    askBatteries,
    askCAR,
    askFRK,
    Batteries (..),
    readBatteries,
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
    frk :: !(Maybe Bool),
    car :: !(Maybe Bool)
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
initialState = BombState Nothing Nothing Nothing Nothing 0 Nothing Nothing

data Batteries = LessThanTwo | Two | MoreThanTwo deriving (Show)

readBatteries :: String -> Bomb (Maybe Batteries)
readBatteries = \case
  "gt2" -> Just MoreThanTwo <$ modify' \s -> s {batteries = Just MoreThanTwo}
  ">2" -> Just MoreThanTwo <$ modify' \s -> s {batteries = Just MoreThanTwo}
  "2" -> Just Two <$ modify' \s -> s {batteries = Just Two}
  "0" -> Just LessThanTwo <$ modify' \s -> s {batteries = Just LessThanTwo}
  "1" -> Just LessThanTwo <$ modify' \s -> s {batteries = Just LessThanTwo}
  n -> case readMaybe @Int n of
    Just n'
      | n' < 0 -> Nothing <$ output "Can't have negative batteries"
      | n' > 2 -> Just MoreThanTwo <$ modify' \s -> s {batteries = Just MoreThanTwo}
      | otherwise -> error "unreachable"
    Nothing -> Nothing <$ output ("Couldn't decipher number of batteries: " <> n)

askBatteries :: (Batteries -> Bomb ()) -> Bomb ()
askBatteries continue = do
  n <- getChar "How many batteries are there? [0/1/2/3 (or more)] (q to abort)\n"
  case n of
    Just 'q' -> pure ()
    Nothing -> pure ()
    Just n' -> readBatteries [n'] >>= \case
      Nothing -> pure ()
      Just n'' -> continue n''

askYesNo :: String -> (Lens' BombState (Maybe Bool)) -> Bomb () -> Bomb ()
askYesNo message boolLens continue = do
  yesNo <- getChar $ message <> " [y/n] (q to abort)\n"
  case fmap toLower yesNo of
    Nothing -> pure ()
    Just 'y' -> do
      modify' (set boolLens (Just True))
      continue
    Just 'n' -> do
      modify' (set boolLens (Just False))
      continue
    -- if user hits q, abort asking
    Just 'q' -> pure ()
    Just _ -> do
      output "Please enter 'y' or 'n'"
      askYesNo message boolLens continue

askSerialOdd :: Bomb () -> Bomb ()
askSerialOdd =
  askYesNo "Is the last digit of the serial number odd?" $
    lens (fmap not . serialEven) (\st b -> st {serialEven = fmap not b})

askSerialEven :: Bomb () -> Bomb ()
askSerialEven =
  askYesNo "Is the last digit of the serial number even?" $
    lens serialEven (\st b -> st {serialEven = b})

askSerialVowel :: Bomb () -> Bomb ()
askSerialVowel =
  askYesNo "Does the serial number have a vowel?" $
    lens serialVowel (\st b -> st {serialVowel = b})

askParallel :: Bomb () -> Bomb ()
askParallel =
  askYesNo "Does the serial number have a vowel?" $
    lens parallel (\st b -> st {parallel = b})

askFRK :: Bomb () -> Bomb ()
askFRK =
  askYesNo "Is there a lit indicator with the label FRK?" $
    lens frk (\st b -> st {frk = b})

askCAR :: Bomb () -> Bomb ()
askCAR =
  askYesNo "Is there a lit indicator with the label CAR?" $
    lens car (\st b -> st {car = b})
