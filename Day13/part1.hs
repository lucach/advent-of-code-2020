import Data.List.Split

main = do
    content <- readFile "input.txt"
    let depTime = (read $ head $ lines content) :: Int
    let ids = [read id :: Int | id <- splitOn "," $ head $ tail $ lines content, id /= "x"]
    let waits = map (\id -> id - (depTime `mod` id)) ids
    let (wait, index) = minimum $ zip waits [0..]
    print $ wait * (ids !! index)