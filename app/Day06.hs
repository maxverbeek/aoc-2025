import qualified Aoc
import Data.List.Split (wordsBy)
import Data.List (transpose)

main :: IO ()
main = do
  rlines <- reverse <$> Aoc.readLines
  let wordlines = transpose $ map (wordsBy (== ' ')) rlines
  print $ sum $ map solve1 wordlines
  print $ solve2 rlines

combine "*" = foldr (*) 1
combine "+" = foldr (+) 0

dropOnlyWhitespace :: [String] -> [String]
dropOnlyWhitespace = filter (not . all (== ' '))

solve1 (op:nums) = combine op (map read nums)

solve2 ("":_) = 0
solve2 (opline : numlines) = solve1 ((nowhitespace opcol) : transposed) + solve2 (restopcol : restnumlines)
  where
  takecol (x:xs) = x : takeWhile (== ' ') xs
  opcol = takecol opline
  numcol = map (take (length opcol)) numlines
  restopcol = drop (length opcol) opline
  restnumlines = map (drop (length opcol)) numlines
  transposed = dropOnlyWhitespace $ map reverse $ transpose numcol

nowhitespace cs = [c | c <- cs, c /= ' ']
