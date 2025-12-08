import Data.List (sortBy, tails)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

type Point = (Int, Int, Int)
type Distance = Double
type Pair = (Point, Point, Distance)
type UnionFind = M.Map Point Point

makeUnionFind :: [Point] -> UnionFind
makeUnionFind = M.fromList . map (\p -> (p, p))

find :: UnionFind -> Point -> Point
find uf p = case M.lookup p uf of
  Nothing -> p
  Just parent | parent == p -> p
              | otherwise -> find uf parent

union :: UnionFind -> Point -> Point -> UnionFind
union uf p1 p2
  | root1 == root2 = uf
  | otherwise = M.insert root1 root2 uf
  where
    root1 = find uf p1
    root2 = find uf p2

connected :: UnionFind -> Point -> Point -> Bool
connected uf p1 p2 = find uf p1 == find uf p2

countComponents :: UnionFind -> Int
countComponents uf = length . M.elems . M.fromList . map (\p -> (find uf p, ())) $ M.keys uf

componentSizes :: UnionFind -> [Int]
componentSizes uf = M.elems . M.fromListWith (+) . map (\p -> (find uf p, 1)) $ M.keys uf

distance :: Point -> Point -> Distance
distance (x1, y1, z1) (x2, y2, z2) = sqrt . fromIntegral $ dx*dx + dy*dy + dz*dz
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

mulx :: Point -> Point -> Int
mulx (x1, _, _) (x2, _, _) = x1 * x2

allPairs :: [Point] -> [Pair]
allPairs points = [(p1, p2, distance p1 p2) | p1:rest <- tails points, p2 <- rest]

sortedByDistance :: [Point] -> [Pair]
sortedByDistance = sortBy (comparing (\(_, _, d) -> d)) . allPairs

connectPairs :: [Point] -> [Pair] -> (UnionFind, Int)
connectPairs points = foldr tryConnect (makeUnionFind points, 0)
  where
    tryConnect (p1, p2, _) (uf, count)
      | connected uf p1 p2 = (uf, count)
      | otherwise = (union uf p1 p2, count + 1)

connectUntilOne :: UnionFind -> [Pair] -> (UnionFind, Maybe Pair)
connectUntilOne uf pairs
  | countComponents uf == 1 = (uf, Nothing)
  | otherwise = go uf pairs
  where
    go currentUf [] = (currentUf, Nothing)
    go currentUf ((p1, p2, d):rest)
      | connected currentUf p1 p2 = go currentUf rest
      | otherwise = let newUf = union currentUf p1 p2
                    in if countComponents newUf == 1
                       then (newUf, Just (p1, p2, d))
                       else go newUf rest

parseLine :: String -> Point
parseLine line = case map read $ splitOn "," line of
  [x, y, z] -> (x, y, z)
  _ -> error "Invalid line format"

main :: IO ()
main = do
  points <- map parseLine . lines <$> getContents

  let pairs = sortedByDistance points
      (uf, successful) = connectPairs points $ take 1000 pairs
      sizes = componentSizes uf
  print $ product . take 3 $ sortBy (flip compare) sizes

  let (_finalUf, finalConn) = connectUntilOne uf $ drop 1000 pairs

  case finalConn of
    Just (p1, p2, d) -> do
      print $ mulx p1 p2
    Nothing ->
      error "already connected everything"
