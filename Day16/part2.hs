import Data.List
import Data.List.Split
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map

isInRange :: Int -> [Int] -> Bool
isInRange val [lower, upper] = val >= lower && val <= upper

isOutsideConstraints :: [[Int]] -> Int -> Bool
isOutsideConstraints constraints val = not $ any (isInRange val) constraints

parseConstraint :: String -> [Int]
parseConstraint c = map read $ splitOn "-" c

parseRule :: String -> (String, [[Int]])
parseRule rule = (name, constraints) where
    [name, constraintsStr] = splitOn ": " rule
    constraints = map parseConstraint $ splitOn " or " constraintsStr

parseNearbyTicket :: String -> [Int]
parseNearbyTicket t = map read $ splitOn "," t

checkIfAllInRange :: [Int] -> (String, [[Int]]) -> Bool
checkIfAllInRange numbers (ruleName, constraints) = not (any (isOutsideConstraints constraints) numbers)


isValidColumn parsedRules ignoredRules column
    | matchingRules /= 1 = Nothing
    | otherwise = Just (snd (relevantRulesResult !! fromMaybe 0 (findIndex (\(res, idx) -> res == True) relevantRulesResult)))
    where
        rulesResult = map (checkIfAllInRange column) parsedRules
        relevantRulesResult = filter (\(res, idx) -> idx `notElem` ignoredRules) $ zip rulesResult [0..length parsedRules - 1]
        matchingRules = length $ filter (\(res, idx) -> res == True) relevantRulesResult


findRule columns parsedRules indices ignoredRules = (asum results, indices !! fromMaybe 0 (findIndex (/= Nothing) results)) where
    results = map (\idx -> isValidColumn parsedRules ignoredRules (extractCol columns idx)) indices

findRules columns indices ignoredRules parsedRules currentMap
    | null indices = currentMap
    | otherwise = recursiveNewMap where
        (foundRule, foundCol) = findRule columns parsedRules indices ignoredRules
        newMap = case foundRule of
            Nothing -> currentMap
            Just pos -> Map.insert (fst (parsedRules !! pos)) foundCol currentMap
        newIndices = filter (/= foundCol) indices
        newIgnoredRules = fromMaybe 0 foundRule : ignoredRules
        recursiveNewMap = findRules columns newIndices newIgnoredRules parsedRules newMap

extractCol matrix idx = map (!! idx) matrix

main = do
    content <- readFile "input.txt"
    let [rules, myTicket, nearbyTickets] = splitOn [""] $ lines content
    let parsedRules = map parseRule rules
    let myTicketParsed = parseNearbyTicket $ myTicket !! 1
    let allConstraints = concatMap snd parsedRules
    let valuesNearby = map parseNearbyTicket (drop 1 nearbyTickets)
    let validNearby = filter (not . any (isOutsideConstraints allConstraints)) valuesNearby
    let rulesMap = findRules validNearby [0..length (head validNearby) - 1] [] parsedRules Map.empty
    let relevantIndices = Map.elems $ Map.filterWithKey (\k _ -> "departure" `isPrefixOf` k) rulesMap
    print $ product $ map (myTicketParsed !!) relevantIndices
