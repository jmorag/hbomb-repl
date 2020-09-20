module Mazes where

import Data.Array
import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set

data Boundary = Boundary
  { north :: Bool,
    east :: Bool,
    south :: Bool,
    west :: Bool
  }

findPath :: (Int, Int) -> (Int, Int) -> Array (Int, Int) Boundary -> [String]
findPath start dst arr = fromMaybe (error "Couldn't find path") $ go mempty start
  where
    go seen (x, y)
      | (x, y) == dst = Just []
      | Set.member (x, y) seen = Nothing
      | otherwise = case arr ! (x, y) of
        Boundary n e s w ->
          asum $
            map
              ( \(s, loc, b) ->
                  if b
                    then Nothing
                    else (s :) <$> go (Set.insert (x, y) seen) loc
              )
              [ ("up", (x - 1, y), n),
                ("right", (x, y + 1), e),
                ("down", (x + 1, y), s),
                ("left", (x, y - 1), w)
              ]

indicesToMaze :: Map.Map ((Int, Int), (Int, Int)) (Array (Int, Int) Boundary)
indicesToMaze =
  Map.fromList
    [ (((2, 1), (3, 6)), firstMap),
      (((2, 5), (4, 2)), secondMap),
      (((4, 4), (4, 6)), thirdMap),
      (((1, 1), (4, 1)), fourthMap),
      (((3, 5), (6, 4)), fifthMap),
      (((1, 5), (5, 3)), sixthMap),
      (((1, 2), (6, 2)), seventhMap),
      (((1, 4), (4, 3)), eighthMap),
      (((2, 3), (5, 1)), ninthMap)
    ]

firstMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False False True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True True True False,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True False True False,
      Boundary False True False False,
      Boundary False True False True,
      Boundary True False True True,
      Boundary False False True False,
      Boundary False True True False,
      Boundary True False True True,
      Boundary False True False False,
      Boundary False False False True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True True True False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True True True False,
      Boundary False False True True,
      Boundary False True True False,
      Boundary True False True True,
      Boundary False True True False
    ]

secondMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False True True,
      Boundary True False False False,
      Boundary True True True False,
      Boundary True False False True,
      Boundary True False False False,
      Boundary True True True False,
      Boundary True False False True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary True True False False,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary True False True False,
      Boundary False True False False,
      Boundary False False False True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary False True True False,
      Boundary True True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False True True True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary True False True False,
      Boundary False True True False
    ]

thirdMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False False True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary True True False True,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False True True True,
      Boundary True True False True,
      Boundary False True False True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True False False,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary False True True False,
      Boundary False False True True,
      Boundary False True True False
    ]

fourthMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False False True,
      Boundary True True False False,
      Boundary True False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary False True False False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary True True True False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False True True,
      Boundary True False True False,
      Boundary False False True False,
      Boundary True False True False,
      Boundary False True False False,
      Boundary False False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True True True False,
      Boundary True False True True,
      Boundary False True True False,
      Boundary False True True True
    ]

fifthMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False False False,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False False False,
      Boundary False True True False,
      Boundary False True True True,
      Boundary False False False True,
      Boundary True True False False,
      Boundary True False True True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True True True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True False True False,
      Boundary False False True False,
      Boundary True True True False,
      Boundary False True False True,
      Boundary False True True True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary False True True False
    ]

sixthMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True True False True,
      Boundary True False False True,
      Boundary True True False False,
      Boundary True False True True,
      Boundary True False False False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False False False True,
      Boundary False True True False,
      Boundary False True True True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary True True False False,
      Boundary True False False True,
      Boundary False True False False,
      Boundary False True False True,
      Boundary True True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False True True True,
      Boundary False True False True,
      Boundary False False True True,
      Boundary False True False False,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary False True True False,
      Boundary True False True True,
      Boundary False True True False
    ]

seventhMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True True True False,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary True True True False,
      Boundary True False False True,
      Boundary False True True False,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False False False True,
      Boundary True False True False,
      Boundary False True True False,
      Boundary True True False True,
      Boundary False True False True,
      Boundary False True True True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary False False True False,
      Boundary False True True False
    ]

eighthMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True True False True,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True True False False,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False False False True,
      Boundary False False True False,
      Boundary True True True False,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False False True True,
      Boundary True True False False,
      Boundary True False True True,
      Boundary False False True False,
      Boundary False True True False,
      Boundary False True False True,
      Boundary True True False True,
      Boundary False False True True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True True False,
      Boundary False False True True,
      Boundary False False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True True True False
    ]

ninthMap =
  listArray
    ((1, 1), (6, 6))
    [ Boundary True True False True,
      Boundary True False False True,
      Boundary True False True False,
      Boundary True False True False,
      Boundary True False False False,
      Boundary True True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True True True False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False False False True,
      Boundary False False True False,
      Boundary False True True False,
      Boundary True False False True,
      Boundary False True True False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True True False True,
      Boundary True False False True,
      Boundary False True True False,
      Boundary True False True True,
      Boundary False True False False,
      Boundary False True False True,
      Boundary False True False True,
      Boundary False True False True,
      Boundary True False False True,
      Boundary True True False False,
      Boundary False True True True,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary False True True False,
      Boundary False False True True,
      Boundary True True True False
    ]
