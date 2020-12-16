import Data.List.Split

isInRange :: Int -> [Int] -> Bool
isInRange val [lower, upper] = val >= lower && val <= upper

isOutsideConstraints :: [[Int]] -> Int -> Bool
isOutsideConstraints constraints val = not $ any (isInRange val) constraints

parseConstraint :: String -> [Int]
parseConstraint c = map read $ splitOn "-" c

parseRule :: String -> [[Int]]
parseRule rule = constraints where
    [name, constraintsStr] = splitOn ": " rule
    constraints = map parseConstraint $ splitOn " or " constraintsStr

parseNearbyTicket :: String -> [Int]
parseNearbyTicket t = map read $ splitOn "," t

main = do
    content <- readFile "input.txt"
    let [rules, myTicket, nearbyTickets] = splitOn [""] $ lines content
    let allConstraints = concatMap parseRule rules
    let valuesNearby = concatMap parseNearbyTicket (drop 1 nearbyTickets)
    print $ sum [if isOutsideConstraints allConstraints value then value else 0 | value <- valuesNearby]