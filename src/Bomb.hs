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
    askSerialOdd,
    askSerialEven,
    askSerialVowel,
    askParallel,
    askBatteries,
    askCAR,
    askFRK,
    Batteries (..),
    Position (..),
    Label (..),
    output,
    module X,
    runBomb,
    condM,
    (&&&),
    just,
  )
where

import Control.Monad.State.Strict as X
import RIO as X hiding (ask, (&&&))
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
    blackSeq :: !Int,
    seqStage :: !Int
  }
  deriving (Show)

newtype Bomb a = Bomb {unBomb :: InputT (StateT BombState IO) a}
  deriving (Functor, Applicative, Monad, MonadIO)

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
initialState = BombState Nothing Nothing Nothing Nothing 0 Nothing Nothing mempty 0 0 0 0

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

ask :: String -> (Lens' BombState (Maybe a)) -> (Char -> Maybe a) -> (a -> Bool) -> Bomb (Maybe Bool)
ask message l parseResponse f =
  gets (view l) >>= \case
    Just x -> pure (Just (f x))
    Nothing ->
      getChar (message <> " (Press any other key to abort)\n")
        <&> join . fmap parseResponse >>= \case
          Nothing -> pure Nothing
          Just r -> modify' (set l (Just r)) >> pure (Just (f r))

askBatteries :: (Batteries -> Bool) -> Bomb (Maybe Bool)
askBatteries =
  ask
    "How many batteries are there? [0/1/2/3 (or more)]"
    (lens batteries (\st b -> st {batteries = b}))
    \r ->
      if
          | r `elem` ("01" :: String) -> Just LessThanTwo
          | r `elem` ("3456789" :: String) -> Just MoreThanTwo
          | r == '2' -> Just Two
          | otherwise -> Nothing

askBool :: String -> (Lens' BombState (Maybe Bool)) -> Bomb (Maybe Bool)
askBool message l =
  ask
    (message <> " [y/n]")
    l
    \case 'y' -> Just True; 'n' -> Just False; _ -> Nothing
    id

askSerialOdd,
  askSerialEven,
  askSerialVowel,
  askParallel,
  askFRK,
  askCAR ::
    Bomb (Maybe Bool)
askSerialOdd =
  askBool "Is the last digit of the serial number odd?" $
    lens (fmap not . serialEven) (\st b -> st {serialEven = fmap not b})
askSerialEven =
  askBool "Is the last digit of the serial number even?" $
    lens serialEven (\st b -> st {serialEven = b})
askSerialVowel =
  askBool "Does the serial number have a vowel?" $
    lens serialVowel (\st b -> st {serialVowel = b})
askParallel =
  askBool "Does the bomb have a parallel port?" $
    lens parallel (\st b -> st {parallel = b})
askFRK =
  askBool "Is there a lit indicator with the label FRK?" $
    lens frk (\st b -> st {frk = b})
askCAR =
  askBool "Is there a lit indicator with the label CAR?" $
    lens car (\st b -> st {car = b})

condM :: Monad m => [(m (Maybe Bool), m ())] -> m ()
condM [] = error "Reached end of condM without a match"
condM ((cond, action) : rest) =
  cond >>= \case
    Just True -> action
    Just False -> condM rest
    Nothing -> pure ()

(&&&) :: (Monad m) => m (Maybe Bool) -> m (Maybe Bool) -> m (Maybe Bool)
mx &&& my =
  mx >>= \case
    Just True -> my
    Just False -> just False
    Nothing -> pure Nothing

just :: (Applicative m) => a -> m (Maybe a)
just = pure . Just
