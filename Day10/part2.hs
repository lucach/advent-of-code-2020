import Data.List
import qualified Data.Map as Map

findOrZero key map = case Map.lookup key map of
    Nothing -> 0
    Just v -> v

countWays ns ways idx
    | idx == -1 = ways Map.! 0
    | otherwise = countWays ns newMap (idx - 1) where
        currentVal = ns !! idx
        currentWays = sum [findOrZero (currentVal + offset) ways | offset <- [1..3]]
        newMap = Map.insert currentVal currentWays ways
        
main = do
    content <- readFile "input.txt"
    let nsWithoutMax = sort (0: map read (lines content))
    let ns = nsWithoutMax ++ [last nsWithoutMax + 3]
    print (countWays ns (Map.singleton (last ns) 1) (length ns - 2))