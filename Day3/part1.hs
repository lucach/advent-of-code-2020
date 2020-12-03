main = do
    content <- readFile "input.txt"
    let rows = lines content
    let cols = length (head rows)
    let indexes = [mod (x*3) cols | x <- [0..(length rows - 1)]]
    let chars = [row !! index | (row, index) <- zip rows indexes]
    print (length (filter (== '#') chars))
