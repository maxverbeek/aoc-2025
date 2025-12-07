import qualified Data.Set as S
import Aoc ((.+), readAsCoords)

type Loc = (Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let oob = makeoob contents
  let start = head $ readAsCoords (== 'S') contents
  let splitters = readAsCoords (== '^') contents
  print $ solve1 start S.empty (S.fromList splitters) oob

makeoob :: String -> Loc -> Bool
makeoob content = (\(x, y) -> x < 0 || x >= width || y < 0 || y >= height)
  where
  l = lines content
  width = length $ head l
  height = length l


splitleft = (-1, 1)
splitright = (1, 1)
dontsplit = (0, 1)

solve1 :: Loc -> S.Set Loc -> S.Set Loc -> (Loc -> Bool) -> Int
solve1 (sx, sy) beams splitters oob = let (count, _) = go (sx, sy+1) beams in count
  where
  go :: Loc -> S.Set Loc -> (Int, S.Set Loc)
  go pos beams
    | oob pos = (0, beams)
    | pos `elem` beams = (0, beams)
    | pos `elem` splitters = ((lc + rc), rb)
    | otherwise = go pos' beams'
    where
    pos' = pos .+ dontsplit
    pl = pos .+ splitleft
    pr = pos .+ splitright
    beams' = S.insert pos' beams
    (lc, lb) = if pl `elem` beams || oob pl then (0, beams) else let (c, lb) = go (pos .+ splitleft) beams in (1+c, lb)
    (rc, rb) = if pr `elem` beams || oob pr then (0, beams) else let (c, rb) = go (pos .+ splitright) lb in (1+c, rb)
