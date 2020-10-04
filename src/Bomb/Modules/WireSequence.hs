module Bomb.Modules.WireSequence (readSeq, wireSeq) where

import Bomb
import Control.Monad.Trans.Maybe
import qualified RIO.Vector as V

data Color = Red | Blue | Black
  deriving (Show, Eq)

data Dest = A | B | C
  deriving (Show, Eq)

data SeqData = SeqData
  { red :: Vector [Dest],
    blue :: Vector [Dest],
    black :: Vector [Dest]
  }

seqData :: SeqData
seqData =
  SeqData
    { red = V.fromList [[C], [B], [A], [A, C], [B], [A, C], [A, B, C], [A, B], [B]],
      blue = V.fromList [[B], [A, C], [B], [A], [B], [B, C], [C], [A, C], [A]],
      black = V.fromList [[A, B, C], [A, C], [B], [A, C], [B], [B, C], [A, B], [C], [C]]
    }

readSeq :: [String] -> Maybe [(Color, Dest)]
readSeq = traverse \case
  [color, dest] -> liftA2
    (,)
    case color of
      'b' -> pure Blue
      'r' -> pure Red
      'k' -> pure Black
      _ -> Nothing
    case dest of
      'a' -> pure A
      'b' -> pure B
      'c' -> pure C
      _ -> Nothing
  _ -> Nothing

wireSeq :: [(Color, Dest)] -> Bomb ()
wireSeq ws =
  runSeq ws >>= \case
    Nothing -> output "Overflowed wire count"
    Just colors -> do
      output (processColors colors)
      stage <- gets seqStage
      modify' \s -> case stage of
        3 -> s {seqStage = 0, redSeq = 0, blueSeq = 0, blackSeq = 0}
        _ -> s {seqStage = stage + 1}

runSeq :: [(Color, Dest)] -> Bomb (Maybe [(Color, Bool)])
runSeq =
  runMaybeT . mapM \(c, d) -> case c of
    Red -> do
      ix <- gets redSeq
      dests <- MaybeT (pure (red seqData V.!? ix))
      modify' \s -> s {redSeq = ix + 1}
      pure (c, d `elem` dests)
    Blue -> do
      ix <- gets blueSeq
      dests <- MaybeT (pure (blue seqData V.!? ix))
      modify' \s -> s {blueSeq = ix + 1}
      pure (c, d `elem` dests)
    Black -> do
      ix <- gets blackSeq
      dests <- MaybeT (pure (black seqData V.!? ix))
      modify' \s -> s {blackSeq = ix + 1}
      pure (c, d `elem` dests)

processColors :: [(Color, Bool)] -> String
processColors [] = "Continue"
processColors [(_, False)] = "Continue"
processColors [(c, True)] = "Cut the " <> (show c) <> " wire"
processColors [(_, True), (_, True)] = "Cut both wires"
processColors [(c1, True), (c2, False)] =
  if c1 == c2
    then "Cut the first " <> show c1 <> " wire"
    else "Cut the " <> show c1 <> " wire"
processColors [(c1, False), (c2, True)] =
  if c1 == c2
    then "Cut the second " <> show c2 <> " wire"
    else "Cut the " <> show c2 <> " wire"
processColors [(_, False), (_, False)] = "Continue"
processColors [(_, True), (_, True), (_, True)] = "Cut all three wires"
processColors [(c1, True), (c2, True), (c3, False)]
  | c1 == c2 && c2 == c3 = "Cut the first and second " <> show c1 <> " wires"
  | c1 == c2 = "Cut both " <> show c1 <> " wires"
  | c1 == c3 = "Cut the first " <> show c1 <> " wire and the " <> show c2 <> " wire"
  | c2 == c3 = "Cut the " <> show c1 <> " wire and the first " <> show c2 <> " wire"
  | otherwise = "Cut the " <> show c1 <> " and " <> show c2 <> " wires"
processColors [(c1, True), (c2, False), (c3, True)]
  | c1 == c2 && c2 == c3 = "Cut the first and third " <> show c1 <> " wires"
  | c1 == c2 = "Cut the first " <> show c1 <> " wire and the " <> show c3 <> " wires"
  | c1 == c3 = "Cut both " <> show c1 <> " wires"
  | c2 == c3 = "Cut the " <> show c1 <> " wire and the second " <> show c3 <> " wire"
  | otherwise = "Cut the " <> show c1 <> " and " <> show c3 <> " wires"
processColors [(c1, True), (_, False), (_, False)] = "Cut the first " <> show c1 <> " wire"
processColors [(c1, False), (c2, True), (c3, True)]
  | c1 == c2 && c2 == c3 = "Cut the second and third " <> show c2 <> " wires"
  | c2 == c3 = "Cut both " <> show c2 <> " wires"
  | c1 == c2 = "Cut the second " <> show c2 <> " wire and the " <> show c3 <> " wire"
  | c1 == c3 = "Cut the " <> show c2 <> " wire and the second " <> show c3 <> " wire"
  | otherwise = "Cut the " <> show c2 <> " and " <> show c3 <> " wires"
processColors [(c1, False), (c2, True), (c3, False)]
  | c1 == c2 = "Cut the second " <> show c2 <> " wire"
  | c2 == c3 = "Cut the first " <> show c2 <> " wire"
  | otherwise = "Cut the " <> show c2 <> " wire"
processColors [(c1, False), (c2, False), (c3, True)]
  | c1 == c3 && c2 == c3 = "Cut the third " <> show c3 <> " wire"
  | c1 == c3 || c2 == c3 = "Cut the second " <> show c3 <> " wire"
  | otherwise = "Cut the " <> show c3 <> " wire"
processColors [(_, False), (_, False), (_, False)] = "Continue"
processColors _ = "There can only be up to three wires per page"
