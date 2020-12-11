-- A better approach would use an appropriate data structure
-- to support constant time indexing in grids

get grid (i,j)
    | i < 0 || j < 0 || i >= rows || j >= cols = '.'
    | otherwise = (grid !! i) !! j
    where
        rows = length grid
        cols = length (head grid)

countOccupied grid (i,j) = length (filter (=='#') neighbors) where
    neighborsPositions = [(i + x, j + y) | x <- [(-1)..1], y <- [(-1)..1], (x, y) /= (0, 0) ]
    neighbors = map (get grid) neighborsPositions

nextState grid (i, j)
    | current == 'L' && occupied == 0 = '#'
    | current == '#' && occupied >= 4 = 'L'
    | otherwise                       = current
    where  
        occupied = countOccupied grid (i,j)
        current = get grid (i, j)

nextGridState grid = [[nextState grid (i, j) | j <- [0..cols-1]] | i <- [0..rows-1]] where
    rows = length grid
    cols = length (head grid)

fixedPoint f arg
    | res == arg = res
    | otherwise = fixedPoint f res
    where
        res = f arg

main = do
    content <- readFile "input.txt"
    let grid = lines content
    let finalGrid = fixedPoint nextGridState grid
    print (length (filter (=='#') (concat finalGrid)))