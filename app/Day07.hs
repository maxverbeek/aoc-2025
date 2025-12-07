import qualified Data.Set as S

main :: IO ()
main = do
  field <- lines <$> getContents
  let beams = map (== 'S') (head field)
  print $ solve1 beams (tail field)
  print $ sum $ solve2 [ if b then 1 else 0 | b <- beams ] (tail field)

solve1 :: [Bool] -> [String] -> Int
solve1 beams [] = 0
solve1 beams (line : rest) = (length $ filter id split) + solve1 combined rest
  where
  continued  = [ c == '.' && beam | (beam, c) <- zip beams line ]
  split      = [ c == '^' && beam | (beam, c) <- zip beams line ]
  splitleft  = tail split ++ [False]
  splitright = False : init split
  combined   = zipWith3 (\a b c -> a || b || c) splitleft splitright continued


solve2 :: [Integer] -> [String] -> [Integer]
solve2 timelines [] = timelines
solve2 timelines (line : rest) = solve2 combined rest
  where
  continued  = [ if c == '.' then tl else 0 | (tl, c) <- zip timelines line ]
  split      = [ if c == '^' then tl else 0 | (tl, c) <- zip timelines line ]
  shiftleft  = tail split ++ [0]
  shiftright = 0 : init split
  combined   = zipWith3 (\a b c -> a + b + c) shiftleft shiftright continued
