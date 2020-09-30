module Bomb.Modules.MorseCode (morse, omorse) where

import Bomb
import Data.Foldable (find)
import RIO.List (findIndex, sortOn)
import qualified RIO.Map as Map

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
morse s = morseHelper s substringWrap

-- like morse, but without wrapping
omorse :: [String] -> Bomb ()
omorse s = morseHelper s substring

morseHelper :: [String] -> (String -> String -> Bool) -> Bomb ()
morseHelper s f = case traverse (alphabet Map.!?) s of
  Just chars ->
    let opts = Map.filterWithKey (\word _ -> f chars word) possibilities
     in output $ show (sortOn snd $ Map.toList opts)
  Nothing -> output $ "Invalid morse character: " ++ findFailureChar s

-- | Checks if chars appear in the order they were entered in the word.
--
-- Wrapping is allowed, but only once, ex.
--
-- >>> substringWrap "rvecto" "vector"
-- True
-- >>> substringWrap "rvector" "vector"
-- False
substringWrap :: String -> String -> Bool
substringWrap [] _ = True
substringWrap (hd : rest) word = case findIndex (== hd) word of
  Nothing -> False
  Just i -> substring rest (drop i word <> take i word)

substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring (c : cs) (w : ws) =
  if c == w
    then substring cs ws
    else substring (c : cs) ws

findFailureChar :: [String] -> String
findFailureChar s = case find (\letter -> Map.notMember letter alphabet) s of
  Just letter -> letter
  Nothing -> error "Unreachable"
