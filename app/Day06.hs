import Data.List (transpose)
import Data.List.Split (splitWhen)

main :: IO ()
main = do
  maths <- splitWhen (all (== ' ')) <$> transpose <$> lines <$> getContents
  print $ sum $ map solve1 maths
  print $ sum $ map solve2 maths

solve1 :: [String] -> Integer
solve1 xs = combine (head (last xs')) (init xs')
  where
  xs' = transpose xs

solve2 :: [String] -> Integer
solve2 xs = combine (last $ head xs) (map init xs)

combine :: Char -> [String] -> Integer
combine '*' = foldr (*) 1 . map read
combine '+' = foldr (+) 0 . map read
