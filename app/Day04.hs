module Main where

import Aoc ((.+))
import qualified Aoc
import qualified Data.Set as S

main :: IO ()
main = do
  content <- getContents
  let locs = S.fromList $ Aoc.readAsCoords (== '@') content
  print $ length $ solve1 locs
  print $ solve2 0 locs

type Location = (Int, Int)

neighbours p locs = S.fromList [ p .+ d | d <- Aoc.directions8 ] `S.intersection` locs

solve1 :: S.Set Location -> S.Set Location
solve1 locs = S.filter (\p -> length (neighbours p locs) < 4) locs

solve2 :: Int -> S.Set Location -> Int
solve2 n locs = let removed = solve1 locs in if S.null removed then n else solve2 (n + length removed) (locs S.\\ removed)
