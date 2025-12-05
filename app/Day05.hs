import qualified Aoc
import Data.List (sort)

main :: IO ()
main = do
  content <- Aoc.readLines
  let [rangesstr, numbers] = Aoc.splitOn "" content
  let ranges = map parseRange rangesstr
  let ingredients = map read numbers
  print $ solve1 ranges ingredients
  print $ solve2 ranges

parseRange :: String -> Range
parseRange = rangeFromList . map read . Aoc.splitOn '-'

type Range = (Integer, Integer)

rangeFromList :: [Integer] -> Range
rangeFromList [a, b] = (a, b)

includes :: Range -> Integer -> Bool
includes (from, to) elem = elem >= from && elem <= to

data Merged = Separate | Expanded Range

mergeoverlap :: Range -> Range -> Merged
mergeoverlap (a, b) (c, d)
  | c < a = mergeoverlap (c, d) (a, b)
  | b < c = Separate
  | otherwise = Expanded (a, max b d)

flattenRanges :: [Range] -> [Range]
flattenRanges [] = []
flattenRanges ranges = go (sort ranges)
  where
    go [] = []
    go [r] = [r]
    go (r1:r2:rs) = case mergeoverlap r1 r2 of
      Separate -> r1 : go (r2:rs)
      Expanded merged -> go (merged:rs)

width :: (Integer, Integer) -> Integer
width (a, b) = (b + 1) - a

solve1 :: [Range] -> [Integer] -> Int
solve1 ranges ingredients = length $ filter (\i -> any (`includes` i) ranges) ingredients

solve2 :: [Range] -> Integer
solve2 ranges  = sum $ map width $ flattenRanges ranges
