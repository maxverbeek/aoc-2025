module Main where

import qualified Aoc
import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- Aoc.readLines
  print (solve1 input)
  print (solve2 input)

solve1 = sum . map (\(a,b) -> a * 10 + b) . map max2

max2 chars = (first, second)
  where
  first = (maximum . toDigits . init) chars
  lastbit = after first (toDigits chars)
  second = maximum lastbit

after x = tail . dropWhile (\c -> c /= x)

droplast n = reverse . drop n . reverse

maxn :: Int -> [Char] -> [Char]
maxn 0 _ = []
maxn n chars = let first = maximum (droplast (n-1) chars) in first : maxn (n - 1) (after first chars)

toDigits :: String -> [Int]
toDigits = map digitToInt

solve2 :: [[Char]] -> Int
solve2 = sum . map read . map (maxn 12)
