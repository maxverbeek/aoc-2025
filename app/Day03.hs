module Main where

import Aoc (readLines, after, droplast)

main :: IO ()
main = do
  input <- readLines
  print (solve1 input)
  print (solve2 input)

maxn :: Int -> [Char] -> [Char]
maxn 0 _ = []
maxn n chars = let first = maximum (droplast (n-1) chars) in first : maxn (n - 1) (after first chars)

solve1 :: [[Char]] -> Int
solve1 = sum . map (read . maxn 2)

solve2 :: [[Char]] -> Int
solve2 = sum . map (read . maxn 12)
