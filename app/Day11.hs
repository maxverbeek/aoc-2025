import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as Map
import Data.MemoTrie

main :: IO ()
main = do
  graph <- Map.fromList <$> map parse <$> lines <$> getContents
  print $ solve1 graph
  print $ solve2 graph

parse line = let [from, to] = splitOn ": " line in (from, words to)

solve1 graph = pathfind graph "you" "out"

solve2 graph = svrfft * fftdac * dacout + svrdac * dacfft * fftout
  where
    svrfft = pathfind graph "svr" "fft"
    svrdac = pathfind graph "svr" "dac"
    fftdac = pathfind graph "fft" "dac"
    dacfft = pathfind graph "dac" "fft"
    fftout = pathfind graph "fft" "out"
    dacout = pathfind graph "dac" "out"

pathfind graph from to = memoFix go from
  where
    go :: (String -> Int) -> String -> Int
    go rec node
      | node == to = 1
      | otherwise = sum [ rec neighbour | neighbour <- Map.findWithDefault [] node graph ]
