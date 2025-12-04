module Main where

import qualified Aoc
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  content <- Aoc.readLines
  let locs = locations content
  print $ length $ solve1 locs
  print $ solve2 0 locs

type Location = (Int, Int)

(.+) :: Location -> Location -> Location
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

locations :: [String] -> S.Set Location
locations lines = S.fromList [ (x, y) | (y, line) <- zip [0.. ] lines, (x, c) <- zip [0..] line, c == '@' ]

directions :: [Location]
directions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0,0)]

neighbours p locs = S.fromList [ p .+ d | d <- directions ] `S.intersection` locs

solve1 :: S.Set Location -> S.Set Location
solve1 locs = S.filter (\p -> length (neighbours p locs) < 4) locs

solve2 :: Int -> S.Set Location -> Int
solve2 n locs = let removed = solve1 locs in if S.null removed then n else solve2 (n + length removed) (locs S.\\ removed)
