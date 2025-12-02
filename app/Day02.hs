module Main where

import qualified Aoc

parseLine :: String -> [(Int, Int)]
parseLine line = map parsePair (Aoc.splitOn ',' line)
  where
    parsePair s = case Aoc.splitOn '-' s of
      [a, b] -> (read a, read b)
      _ -> error "invalid pair"

main :: IO ()
main = do
  input <- getLine
  let parsed = parseLine input
  print (solve1 parsed)
  print (solve2 parsed)


ids :: (Int, Int) -> [String]
ids (a, b) = [ show x | x <- [a..b] ]

twiceRepeated id = even len && first == second
  where
  len = (length id)
  halflen = len `div` 2
  first = take halflen id
  second = drop halflen id

repeated needle haystack = all id $ zipWith (==) haystack (cycle needle)

manyRepeated :: String -> Bool
manyRepeated id = any (\x -> x) repeats
  where
  len = length id
  repeats = [ repeated (take c id) (drop c id) | c <- [1..(len `div` 2)], len `mod` c == 0 ]
  

solve1 :: [(Int, Int)] -> Int
solve1 = sum . map read . filter twiceRepeated . concatMap ids

solve2 :: [(Int, Int)] -> Int
solve2 = sum . map read . filter manyRepeated . concatMap ids
