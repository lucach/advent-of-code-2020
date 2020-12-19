import Data.List
import Data.List.Split
import qualified Data.Map as Map


resolveSequence rules indices = map concat $ sequence $ map (resolvePattern rules) indices

resolvePattern rules idx 
    | not (null currentPattern) = currentPattern
    | otherwise = concat [resolveSequence rules alternative | alternative <- alternatives]
    where
        (currentPattern, alternatives) = rules Map.! idx

parseRuleNumber :: String -> [[Int]]
parseRuleNumber rule = allRulesNumbers where
    alternatives = splitOn "|" rule
    allRulesNumbers = [map read $ words alternative | alternative <- alternatives] :: [[Int]]

parseRule :: String -> (Int, [String], [[Int]])
parseRule rule = (read indexStr, letters, references) where
    [indexStr, rest] = splitOn ":" rule
    (letters, references) = case elemIndex '"' rule of
        Nothing -> ([], parseRuleNumber rest)
        _ -> ([read rest], [])

main = do
    content <- readFile "input.txt"
    let [rules, messages] = splitOn [""] $ lines content
    let parsedRules = map parseRule rules
    let rulesMap = foldl (\m (idx, ls, refs) -> Map.insert idx (ls, refs) m) Map.empty parsedRules
    let patterns = resolvePattern rulesMap 0
    print $ length $ filter (`elem` patterns) messages