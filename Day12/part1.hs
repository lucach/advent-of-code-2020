getAction :: String -> (Char, Int)
getAction str = (head str, read $ tail str)

moveInDir (posX, posY, value, dir) 
    | dir == 3 = (posX + value, posY)
    | dir == 1 = (posX - value, posY)
    | dir == 0 = (posX, posY + value)
    | dir == 2 = (posX, posY - value)

updateDirByDegrees fn dir deg = (fn dir (deg `div` 90) + 4) `mod` 4

move ((posX, posY), dir) (action, value)
    | action == 'N' = (moveInDir (posX, posY, value, 3), dir)
    | action == 'S' = (moveInDir (posX, posY, value, 1), dir)
    | action == 'E' = (moveInDir (posX, posY, value, 0), dir)
    | action == 'W' = (moveInDir (posX, posY, value, 2), dir)
    | action == 'L' = ((posX, posY), updateDirByDegrees (-) dir value)
    | action == 'R' = ((posX, posY), updateDirByDegrees (+) dir value)
    | action == 'F' = (moveInDir (posX, posY, value, dir), dir)

main = do
    content <- readFile "input.txt"
    let actions = map getAction $ lines content
    let ((finalX, finalY), _) = foldl move ((0, 0), 0) actions
    print $ abs finalX + abs finalY
