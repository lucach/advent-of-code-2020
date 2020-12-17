import Data.List.Split

gridLimits grid = (maxX, maxY, maxZ, maxW) where
    maxW = length grid - 1
    maxZ = length (head grid) - 1
    maxY = length (head (head grid)) - 1
    maxX = length (head (head (head grid))) - 1

getAt grid (x, y, z, w)
    | withinBoundaries = (((grid !! w) !! z) !! y) !! x
    | otherwise = '.'
    where
        (maxX, maxY, maxZ, maxW) = gridLimits grid
        withinBoundaries = w >= 0 && w <= maxW && z >= 0 && z <= maxZ && y >= 0 && y <= maxY && x >= 0 && x <= maxX

getNeighbors x y z w grid = map (getAt grid) neighborsPos where
    neighborsPos = [(x + deltaX, y + deltaY, z + deltaZ, w + deltaW) | deltaX <- [-1..1], deltaY <- [-1..1], deltaZ <- [-1..1], deltaW <- [-1..1], (deltaX, deltaY, deltaZ, deltaW) /= (0,0,0,0)]

nextState grid (x, y, z, w)
    | currentState == '#' && (activeNeighbors == 2  || activeNeighbors == 3) = '#'
    | currentState == '#' = '.'
    | currentState == '.' && activeNeighbors == 3 = '#'
    | currentState == '.' = '.'
    where
        currentState = getAt grid (x, y, z, w)
        activeNeighbors = length $ filter (=='#') $ getNeighbors x y z w grid

nextGrid grid = properLayers where
    (maxX, maxY, maxZ, maxW) = gridLimits grid
    allPos = [(x, y, z, w) | w <- [-1..maxW + 1], z <- [-1..maxZ + 1], y <- [-1..maxY + 1], x <- [-1..maxX + 1]]
    flatGrid = map (nextState grid) allPos
    layersW = chunksOf ((maxZ + 3) * (maxX + 3) * (maxY + 3)) flatGrid
    layers = map (chunksOf ((maxX + 3) * (maxY + 3))) layersW
    properLayers = map (map (chunksOf (maxX + 3))) layers

nextGridTimes grid times
    | times == 0 = grid
    | otherwise  = nextGridTimes (nextGrid grid) (times - 1)

main = do
    content <- readFile "input.txt"
    let grid = [[lines content]]
    let finalGrid = nextGridTimes grid 6
    print $ length $ filter (=='#') $ concat $ concat $ concat finalGrid