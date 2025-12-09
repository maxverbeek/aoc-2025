import qualified Aoc
import Data.List (tails)

main :: IO ()
main = do
  coords <- map ((\[x, y] -> (read x, read y)) . Aoc.splitOn ',') <$> Aoc.readLines
  print $ solve1 coords
  print $ solve2 coords

type Coord = (Int, Int)

surface (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

solve1 :: [Coord] -> Int
solve1 coords = maximum [ surface p1 p2 | p1:rest <- tails coords, p2 <- rest]

solve2 coords = maximum [ surface p1 p2 | p1:rest <- tails coords, p2 <- rest, nointersections p1 p2, nointersections p2 p1 ]
  where
  nointersections a b = not $ any (edgeCrosses xl xh yl yh) edges
    where
    vertices = takeWhile (/= b) $ dropWhile (/= a) (cycle coords)
    edges = zip vertices (tail vertices)
    ((xl, yl), (xh, yh)) = hlhl a b

  edgeCrosses xl xh yl yh ((x1, y1), (x2, y2))
    | x1 == x2 = x1 > xl && x1 < xh && min y1 y2 < yh && max y1 y2 > yl
    | y1 == y2 = y1 > yl && y1 < yh && min x1 x2 < xh && max x1 x2 > xl
    | otherwise = error "cannot have diagonal edges"

hlhl (x1, y1) (x2, y2) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
