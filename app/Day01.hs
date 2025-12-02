module Main where

import qualified Aoc

parseLine :: String -> Maybe (Aoc.LetterDirection, Int)
parseLine (d : rest) = do
  direction <- Aoc.intoLetterDirection d
  number <- readMaybe rest
  return (direction, number)
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing
parseLine _ = Nothing

main :: IO ()
main = do
  inputLines <- Aoc.readLines
  case mapM parseLine inputLines of
    Just items -> do
      putStrLn $ "Part 1: " ++ show (solve1 items)
      putStrLn $ "Part 2: " ++ show (solve2 items)
    Nothing -> putStrLn "parse error"

data Dial = Dial Int deriving (Show, Eq)

-- Move the dial based on direction and amount
move :: Dial -> (Aoc.LetterDirection, Int) -> Dial
move (Dial pos) (Aoc.L, n) = Dial ((pos - n) `mod` 100)
move (Dial pos) (Aoc.R, n) = Dial ((pos + n) `mod` 100)

moveslow :: Dial -> (Aoc.LetterDirection, Int) -> [Dial]
moveslow dial (_, 0) = []
moveslow dial (dir, count) =
  let next = move dial (dir, 1)
   in next : moveslow next (dir, count - 1)

solve1 :: [(Aoc.LetterDirection, Int)] -> Int
solve1 = length . filter (== Dial 0) . scanl move (Dial 50)

solve2 :: [(Aoc.LetterDirection, Int)] -> Int
solve2 items =
  let dials = scanl move (Dial 50) items
      sublists = zipWith moveslow dials items
   in sum $ map (length . filter (== Dial 0)) sublists 
