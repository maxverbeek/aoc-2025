import qualified Aoc

main :: IO ()
main = do
  content <- Aoc.readLines
  let [rangesstr, numbers] = Aoc.splitOn "" content
  let ranges = flattenRanges $ map parseRange rangesstr
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

subtractRange :: Range -> Range -> [Range]
subtractRange (a, b) (c, d) =
  [ (a, c - 1) | (a, b) `includes` c ] ++
  [ (d + 1, b) | (a, b) `includes` d ] ++
  [ (a, b) | d < a || c > b ]

flattenRanges = foldr add []
  where add range rest = range : concatMap (`subtractRange` range) rest

width :: (Integer, Integer) -> Integer
width (a, b) = (b + 1) - a

solve1 :: [Range] -> [Integer] -> Int
solve1 ranges ingredients = length $ filter (\i -> any (`includes` i) ranges) ingredients

solve2 :: [Range] -> Integer
solve2 ranges  = sum $ map width ranges
