import Data.List
import qualified Data.Map as Map

findOrZero key map = case Map.lookup key map of
    Nothing -> 0
    Just v -> v

countWays ns ways idx
    | idx == (length ns) = ways Map.! 0
    | otherwise = countWays ns newMap (idx + 1) where
        currentVal = ns !! idx
        currentWays = sum [findOrZero (currentVal + offset) ways | offset <- [1..3]]
        newMap = Map.insert currentVal currentWays ways
        
main = do
    content <- readFile "input.txt"
    let nsWithoutMax = sort (0: map read (lines content))
    let ns = (last nsWithoutMax + 3 : reverse nsWithoutMax)
    print (countWays ns (Map.singleton (head ns) 1) 1)