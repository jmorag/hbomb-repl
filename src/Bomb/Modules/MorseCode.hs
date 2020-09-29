module Bomb.Modules.MorseCode (morse, omorse) where

import Bomb
import Data.Foldable (find)
import RIO.Char
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- Does not include alphabet letters that appear in no words in possibilities
alphabet :: Map.Map String Char
alphabet =
  Map.fromList
    [ (".-", 'a'),
      ("-...", 'b'),
      ("-.-.", 'c'),
      (".", 'e'),
      ("..-.", 'f'),
      ("--.", 'g'),
      ("....", 'h'),
      ("..", 'i'),
      ("-.-", 'k'),
      (".-..", 'l'),
      ("--", 'm'),
      ("-.", 'n'),
      ("---", 'o'),
      (".-.", 'r'),
      ("...", 's'),
      ("-", 't'),
      ("...-", 'v'),
      ("-..-", 'x')
    ]

possibilities :: Map.Map String Double
possibilities =
  Map.fromList
    [ ("shell", 3.505),
      ("halls", 3.515),
      ("slick", 3.522),
      ("trick", 3.532),
      ("boxes", 3.535),
      ("leaks", 3.542),
      ("strobe", 3.545),
      ("bistro", 3.552),
      ("flick", 3.555),
      ("bombs", 3.565),
      ("break", 3.572),
      ("brick", 3.575),
      ("steak", 3.582),
      ("sting", 3.592),
      ("vector", 3.595),
      ("beats", 3.600)
    ]

morse :: [String] -> Bomb ()
morse s = case traverse (alphabet Map.!?) s of
  Just chars ->
    let opts = Map.filterWithKey (\word _ -> all (\c -> c `elem` word) chars) possibilities
     in output $ show opts
  Nothing -> output $ "Invalid morse character: " ++ findFailureChar s

isOrderedSubstring :: String -> String -> Bool
isOrderedSubstring [] _ = True
isOrderedSubstring _ [] = False
isOrderedSubstring (c : cs) (w : ws) =
  if c == w
    then isOrderedSubstring cs ws
    else isOrderedSubstring (c : cs) ws

--- This is ordered morse. Letters must be in order.
omorse :: [String] -> Bomb ()
omorse s = case traverse (alphabet Map.!?) s of
  Just chars ->
    let opts = Map.filterWithKey (\word _ -> isOrderedSubstring chars word) possibilities
     in output $ show opts
  Nothing -> output $ "Invalid morse character: " ++ findFailureChar s

findFailureChar :: [String] -> String
findFailureChar s = case find (\letter -> Map.notMember letter alphabet) s of
  Just letter -> letter
  Nothing -> error "Unreachable"
