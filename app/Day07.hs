import qualified Data.Set as S
import Debug.Trace

main :: IO ()
main = do
  field <- lines <$> getContents
  let beams = map (== 'S') (head field)
  print $ solve1 beams (tail field)
  print $ sum $ solve2 [ if b then 1 else 0 | b <- beams ] (tail field)

solve1 :: [Bool] -> [String] -> Int
solve1 beams [] = 0
solve1 beams (line : rest) = (length splits) + solve1 newbeams rest
  where
  splits = [ [ i-1, i+1 ] | (i, beam, splitter) <- zip3 [0..] beams line, beam, splitter == '^' ]
  splitbeams = S.filter (\l -> l >= 0 && l < length line) $ S.fromList $ concat splits
  continues = S.fromList [ i | (i, air, beam) <- zip3 [0..] line beams, beam, air == '.' ]
  newbeams = [ loc `elem` splitbeams || loc `elem` continues | (loc, _) <- zip [0..] line ]

solve2 :: [Integer] -> [String] -> [Integer]
solve2 timelines [] = timelines
solve2 timelines (line : rest) = solve2 combined rest
  where
  continued  = [ if c == '.' then tl else 0 | (tl, c) <- zip timelines line ]
  split      = [ if c == '^' then tl else 0 | (tl, c) <- zip timelines line ]
  shiftleft  = tail split ++ [0]
  shiftright = 0 : init split
  combined   = zipWith3 (\a b c -> a + b + c) shiftleft shiftright continued
