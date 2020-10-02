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
    withSerialOdd,
    withSerialEven,
    withSerialVowel,
    withParallel,
    withBatteries,
    withCAR,
    withFRK,
    Batteries (..),
    Position (..),
    Label (..),
    output,
    module X,
    runBomb,
  )
where

import Control.Monad.State.Strict as X
import RIO as X
import System.Console.Haskeline

data BombState = BombState
  { batteries :: !(Maybe Batteries),
    serialEven :: !(Maybe Bool),
    serialVowel :: !(Maybe Bool),
    parallel :: !(Maybe Bool),
    strikes :: !Int,
    frk :: !(Maybe Bool),
    car :: !(Maybe Bool),
    memory :: !(Seq (Position, Label)),
    redSeq :: !Int,
    blueSeq :: !Int,
    blackSeq :: !Int
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
initialState = BombState Nothing Nothing Nothing Nothing 0 Nothing Nothing mempty 0 0 0

data Batteries = LessThanTwo | Two | MoreThanTwo deriving (Show, Eq)

newtype Position = Position Int
  deriving (Eq, Show)

instance Num Position where
  fromInteger i =
    if i `elem` [1 .. 4]
      then Position (fromIntegral i)
      else error "Position must be 1,2,3, or 4"
  (+) = error "Not implemented"
  (*) = error "Not implemented"
  abs = error "Not implemented"
  signum = error "Not implemented"
  negate = error "Not implemented"

newtype Label = Label Int
  deriving (Eq, Show)

instance Num Label where
  fromInteger i =
    if i `elem` [1 .. 4]
      then Label (fromIntegral i)
      else error "Label must be 1,2,3, or 4"
  (+) = error "Not implemented"
  (*) = error "Not implemented"
  abs = error "Not implemented"
  signum = error "Not implemented"
  negate = error "Not implemented"

withPossiblyUnknown :: String -> (Lens' BombState (Maybe a)) -> (Char -> Maybe a) -> (a -> Bomb ()) -> Bomb ()
withPossiblyUnknown message len parseResponse cont =
  gets (view len) >>= \case
    Just x -> cont x
    Nothing ->
      getChar (message <> " (Press any other key to abort)\n") <&> join . fmap parseResponse >>= \case
        Nothing -> pure ()
        Just r -> modify' (set len (Just r)) >> cont r

withBatteries :: (Batteries -> Bomb ()) -> Bomb ()
withBatteries =
  withPossiblyUnknown
    "How many batteries are there? [0/1/2/3 (or more)]"
    (lens batteries (\st b -> st {batteries = b}))
    \r ->
      if
          | r `elem` ("01" :: String) -> Just LessThanTwo
          | r `elem` ("3456789" :: String) -> Just MoreThanTwo
          | r == '2' -> Just Two
          | otherwise -> Nothing

withBool :: String -> (Lens' BombState (Maybe Bool)) -> (Bool -> Bomb ()) -> Bomb ()
withBool message len =
  withPossiblyUnknown (message <> " [y/n]") len \case
    'y' -> Just True
    'n' -> Just False
    _ -> Nothing

withSerialOdd,
  withSerialEven,
  withSerialVowel,
  withParallel,
  withFRK,
  withCAR ::
    (Bool -> Bomb ()) -> Bomb ()
withSerialOdd =
  withBool "Is the last digit of the serial number odd?" $
    lens (fmap not . serialEven) (\st b -> st {serialEven = fmap not b})
withSerialEven =
  withBool "Is the last digit of the serial number even?" $
    lens serialEven (\st b -> st {serialEven = b})
withSerialVowel =
  withBool "Does the serial number have a vowel?" $
    lens serialVowel (\st b -> st {serialVowel = b})
withParallel =
  withBool "Does the bomb have a parallel port?" $
    lens parallel (\st b -> st {parallel = b})
withFRK =
  withBool "Is there a lit indicator with the label FRK?" $
    lens frk (\st b -> st {frk = b})
withCAR =
  withBool "Is there a lit indicator with the label CAR?" $
    lens car (\st b -> st {car = b})
