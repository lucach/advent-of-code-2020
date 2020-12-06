import Data.List
import Data.List.Split

findGroups ls = map (splitOn " ") (splitOn "  " (intercalate " " ls))

testLetters g = [map (elem letter) g | letter <- ['a'..'z']]

findAllTrue = map (all (==True))

countAll lettersTests = length (filter (==True) lettersTests)

main = do
    content <- readFile "input.txt"
    let groups = findGroups (lines content)
    let counts = map (countAll . findAllTrue . testLetters) groups
    print (sum counts)