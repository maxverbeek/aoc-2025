module Aoc where

import Data.Char (digitToInt)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

data LetterDirection = L | R deriving (Show)

intoLetterDirection :: Char -> Maybe LetterDirection
intoLetterDirection 'L' = Just L
intoLetterDirection 'R' = Just R
intoLetterDirection _ = Nothing

readLines :: IO [String]
readLines = lines <$> getContents

readAsCoords :: (Char -> Bool) -> String -> [(Int, Int)]
readAsCoords predicate input =
  [ (x, y)
  | (y, line) <- zip [0 ..] $ lines input,
    (x, char) <- zip [0 ..] line,
    predicate char
  ]

directions4 :: [(Int, Int)]
directions4 = [(-1, 0), (1, 0), (0, -1), (0, 1)]

directions8 :: [(Int, Int)]
directions8 = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
  (a, []) -> [a]
  (a, _ : b) -> a : splitOn delim b

after :: (Eq a) => a -> [a] -> [a]
after x = tail . dropWhile (/= x)

droplast :: Int -> [a] -> [a]
droplast n = reverse . drop n . reverse

toDigits :: String -> [Int]
toDigits = map digitToInt

-- a represents a state
-- given an initial state, and a neighbour function, and a goal predicate, give me the shortest path of all states to my goal.
bfs :: (Ord a) => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
bfs start neighbours isGoal = go (Seq.singleton [start]) (Set.singleton start)
  where
    go Empty _ = Nothing
    go (path :<| queue) visited
      | isGoal current = Just $ reverse path
      | otherwise = go queue' visited'
      where
        current = head path
        nexts = filter (`Set.notMember` visited) (neighbours current)
        newPaths = map (: path) nexts
        queue' = foldl (|>) queue newPaths
        visited' = foldl (flip Set.insert) visited nexts
