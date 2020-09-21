module Bomb.Modules.Passwords (passwords) where

import Bomb

possibilities :: [String]
possibilities =
  [ "about",
    "after",
    "again",
    "below",
    "could",
    "every",
    "first",
    "found",
    "great",
    "house",
    "large",
    "learn",
    "never",
    "other",
    "place",
    "plant",
    "point",
    "right",
    "small",
    "sound",
    "spell",
    "still",
    "study",
    "their",
    "there",
    "these",
    "thing",
    "think",
    "three",
    "water",
    "where",
    "which",
    "world",
    "would",
    "write"
  ]

passwords :: [String] -> Bomb ()
passwords input = output (show (filter (match input) possibilities))

match :: [String] -> String -> Bool
match input pwd = and $ zipWith matcher input pwd
  where
    matcher "-" _ = True
    matcher "_" _ = True
    matcher inp p = p `elem` inp
