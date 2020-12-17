import Data.List.Split

gridLimits grid = (maxX, maxY, maxZ) where
    maxZ = length grid - 1
    maxY = length (head grid) - 1
    maxX = length (head (head grid)) - 1

getAt grid (x, y, z)
    | withinBoundaries = ((grid !! z) !! y) !! x
    | otherwise = '.'
    where
        (maxX, maxY, maxZ) = gridLimits grid
        withinBoundaries = z >= 0 && z <= maxZ && y >= 0 && y <= maxY && x >= 0 && x <= maxX

getNeighbors x y z grid = map (getAt grid) neighborsPos where
    neighborsPos = [(x + deltaX, y + deltaY, z + deltaZ) | deltaX <- [-1..1], deltaY <- [-1..1], deltaZ <- [-1..1], (deltaX, deltaY, deltaZ) /= (0,0,0)]

nextState grid (x, y, z)
    | currentState == '#' && (activeNeighbors == 2  || activeNeighbors == 3) = '#'
    | currentState == '#' = '.'
    | currentState == '.' && activeNeighbors == 3 = '#'
    | currentState == '.' = '.'
    where
        currentState = getAt grid (x, y, z)
        activeNeighbors = length $ filter (=='#') $ getNeighbors x y z grid

nextGrid grid = properLayers where
    (maxX, maxY, maxZ) = gridLimits grid
    allPos = [(x, y, z) | z <- [-1..maxZ + 1], y <- [-1..maxY + 1], x <- [-1..maxX + 1]]
    flatGrid = map (nextState grid) allPos
    layers = chunksOf ((maxX + 3) * (maxY + 3)) flatGrid
    properLayers = map (chunksOf (maxX + 3)) layers

nextGridTimes grid times
    | times == 0 = grid
    | otherwise  = nextGridTimes (nextGrid grid) (times - 1)

main = do
    content <- readFile "input.txt"
    let grid = [lines content]
    let finalGrid = nextGridTimes grid 6
    print $ length $ filter (=='#') $ concat $ concat finalGrid