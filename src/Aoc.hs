module Aoc where
import Data.Char (digitToInt)

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

after :: Eq a => a -> [a] -> [a]
after x = tail . dropWhile (\c -> c /= x)

droplast :: Int -> [a] -> [a]
droplast n = reverse . drop n . reverse

toDigits :: String -> [Int]
toDigits = map digitToInt
