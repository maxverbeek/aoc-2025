import Data.Char (isDigit)
import Data.List (isSuffixOf)

import Debug.Trace

main :: IO ()
main = do
  (presents, problems) <- parse <$> lines <$> getContents
  print $ count $ map (solve1 presents) problems

type Problem = ((Int, Int), [Int])

count xs = length $ filter (== True) xs

parse :: [String] -> ([[String]], [Problem])
parse input = parse' input [] []
  where
  parse' [] presents problems = (reverse presents, reverse problems)
  parse' (line:rest) presents problems
    | line == "" = parse' rest presents problems
    | ":"`isSuffixOf` line = parse' (dropWhile (/= "") rest) (present : presents) problems
    | otherwise = parse' rest presents (problem : problems)
    where
    present = takeWhile (/= "") rest
    problem = ((width, height), map read indeces)
      where
      (dims:indeces) = words line
      width = read (takeWhile isDigit dims)
      height = read $ init $ tail $ dropWhile isDigit dims

solve1 presents ((width, height), counts) = width * height >= 9 * (sum counts)
