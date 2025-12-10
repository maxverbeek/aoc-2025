import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Aoc
import Data.SBV
import Data.SBV.Control
import Control.Monad (unless, forM_)

main :: IO ()
main = do
  input <- map parse <$> lines <$> getContents
  print $ sum $ map (\(lights, buttons, _) -> solve1 lights buttons) input
  results <- mapM (\(_, buttons, joltage) -> solve2 joltage buttons) input
  print $ sum results

parse :: String -> ([Bool], [[Integer]], [Integer])
parse = assign . splitOn " "
  where
  indeces = map (== '#') . init . tail
  numbers = map read . splitOn "," . init . tail
  assign (first:rest) = ((indeces first), (map numbers $ init rest), (numbers $ last rest))

solve1 goal buttons = case (Aoc.bfs [False | _ <- goal] neighbours (== goal)) of
    Just steps -> length steps - 1
    Nothing -> error "no path found"

  where
  neighbours :: [Bool] -> [[Bool]]
  neighbours cur = map (cur `toggleAt`) buttons
  toggleAt state locations = [ if i `elem` locations then not c else c | (i, c) <- zip [0..] state]

solve2 goal buttons = minVectorCombination joltageincrements goal
  where
  joltageincrements = map (\locations -> [ if i `elem` locations then 1 else 0 | (i, _) <- zip [0..] goal ]) buttons


-- Solve: minimal number of vector additions to reach target
minVectorCombination 
    :: [[Integer]]  -- list of vectors
    -> [Integer]    -- target vector
    -> IO Integer
minVectorCombination vectors target = do
    let dim = length target
        n   = length vectors

    unless (all ((== dim) . length) vectors) $
        error "All vectors must have the same dimension as the target"

    -- Solve and get the optimization result
    result <- optLexicographic $ do
        cs <- mapM (\i -> sInteger ("c" ++ show i)) [1 .. n]

        -- non-negativity
        mapM_ (\c -> constrain (c .>= 0)) cs

        -- vector sum constraint
        forM_ [0 .. dim-1] $ \j -> do
            let lhs = sum
                    [ fromInteger (vectors !! i !! j) * cs !! i
                    | i <- [0 .. n-1]
                    ]
            constrain $ lhs .== fromInteger (target !! j)

        -- the objective
        minimize "total" (sum cs)

    -- Extract named objective "total"
    case getModelValue "total" result of
        Just x -> pure x
        Nothing -> error "No objective value returned!"

-- solve2 goal buttons =  go (M.fromList $ zip joltageincrements [1..])
--   where
--   joltageincrements = map (\locations -> [ if i `elem` locations then 1 else 0 | (i, _) <- zip [0..] goal ]) buttons
--   go buttons
--     | Just steps <- goal `M.member` buttons = steps
--     | otherwise = go addyourself
--     where
--     addyourself = M.unionWith min buttons (M.fromList nextbuttons)
--     nextbuttons = [ (next, steps1 + steps2)
--       | (button1, steps1) <- M.toList buttons
--       , (button2, steps2) <- M.toList buttons
--       , let next = zipWith (+) button1 button2,
--       and [ c <= g | (c, g) <- zip next goal ]
--       ]

-- solve2bfs goal buttons = case Aoc.bfs [0 | _ <- goal] neighbours (== goal) of
--   Just steps -> length steps - 1
--   Nothing -> error "no path found"
--   where
--   neighbours :: [Int] -> [[Int]]
--   neighbours cur = filter lessthangoal $ map (zipWith (+) cur) joltageincrements
--   lessthangoal :: [Int] -> Bool
--   lessthangoal state = and [ c <= g | (c, g) <- zip state goal]
