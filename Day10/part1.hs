import Data.List

findDiffs ns goal
    | diffToGoal <= 3 = [diffToGoal]
    | otherwise = (second - first : findDiffs (tail ns) goal)
    where
        first = head ns
        second = head (tail ns)
        diffToGoal = goal - first

count list elem = length (filter (==elem) list)

main = do
    content <- readFile "input.txt"
    let ns = sort (map read (lines content))
    let goal = last ns + 3
    let diffs = findDiffs (0:ns) goal
    let diff13 = map (count diffs) [1, 3]
    print (product diff13)