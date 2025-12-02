module Aoc (LetterDirection (..), intoLetterDirection, readLines, splitOn) where

data LetterDirection = L | R deriving Show

intoLetterDirection 'L' = Just L
intoLetterDirection 'R' = Just R
intoLetterDirection x = Nothing

readLines :: IO [String]
readLines = lines <$> getContents

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
  (a, []) -> [a]
  (a, _:b) -> a : splitOn delim b
