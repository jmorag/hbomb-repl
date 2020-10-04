module Bomb.Modules.SimonSays (readSimon, simon, simonAll) where

import Bomb
import qualified RIO.Map as M

data Color = Red | Blue | Green | Yellow
  deriving (Show, Eq, Ord)

type Strikes = Int

type SerialVowel = Bool

simonData :: M.Map (Color, Strikes, SerialVowel) Color
simonData =
  M.fromList
    [ ((Red, 0, True), Blue),
      ((Blue, 0, True), Red),
      ((Green, 0, True), Yellow),
      ((Yellow, 0, True), Green),
      ((Red, 1, True), Yellow),
      ((Blue, 1, True), Green),
      ((Green, 1, True), Blue),
      ((Yellow, 1, True), Red),
      ((Red, 2, True), Green),
      ((Blue, 2, True), Red),
      ((Green, 2, True), Yellow),
      ((Yellow, 2, True), Blue),
      ((Red, 0, False), Blue),
      ((Blue, 0, False), Yellow),
      ((Green, 0, False), Green),
      ((Yellow, 0, False), Red),
      ((Red, 1, False), Red),
      ((Blue, 1, False), Blue),
      ((Green, 1, False), Yellow),
      ((Yellow, 1, False), Green),
      ((Red, 2, False), Yellow),
      ((Blue, 2, False), Green),
      ((Green, 2, False), Blue),
      ((Yellow, 2, False), Red)
    ]

readSimon :: String -> Maybe [Color]
readSimon = traverse \case
  'r' -> Just Red
  'b' -> Just Blue
  'g' -> Just Green
  'y' -> Just Yellow
  _ -> Nothing

simon :: [Color] -> Bomb ()
simon colors = do
  strike <- gets strikes
  askSerialVowel >>= maybe (pure ()) \vowel ->
    output . maybe "Unknown color" show $
      traverse (\c -> simonData M.!? (c, strike, vowel)) colors

simonAll :: Bomb ()
simonAll = do
  strike <- gets strikes
  askSerialVowel >>= maybe (pure ()) \vowel ->
    output case traverse (\c -> simonData M.!? (c, strike, vowel)) topRow of
      Just result@[_, _, _, _] ->
        let top = format topRow
            dashes = replicate (length top) '-'
         in dashes <> "\n" <> top <> "\n" <> dashes <> "\n" <> format result <> "\n" <> dashes
      _ -> "Invalid number of strikes. Check bomb state with `dump`"

topRow :: [Color]
topRow = [Red, Blue, Green, Yellow]

format :: [Color] -> String
format = foldr go "|"
  where
    go color acc =
      "| "
        <> case color of Red -> "R "; Blue -> "B "; Green -> "G "; Yellow -> "Y "
        <> acc
