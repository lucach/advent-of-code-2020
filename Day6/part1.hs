import Data.List
import Data.List.Split

findGroups ls = splitOn "  " (intercalate " " ls)

uniqueAnswers = map (nub . filter (/= ' '))

main = do
    content <- readFile "input.txt"
    let groups = findGroups (lines content)
    let answers = uniqueAnswers groups
    print (sum (map length answers))