getAction :: String -> (Char, Int)
getAction str = (head str, read $ tail str)

moveWayInDir (wayX, wayY) dir value 
    | dir == 3 = (wayX, wayY + value)
    | dir == 1 = (wayX, wayY - value)
    | dir == 0 = (wayX + value, wayY)
    | dir == 2 = (wayX - value, wayY)

rotateBy (wayX, wayY) deg
    | deg == 0 = (wayX, wayY)
    | deg < 0  = rotateBy (wayX, wayY) (360 - (-deg))
    | deg > 0 = rotateBy (wayY, -wayX) (deg - 90)

move ((posX, posY), (wayX, wayY)) (action, value)
    | action == 'N' = ((posX, posY), moveWayInDir (wayX, wayY) 3 value)
    | action == 'S' = ((posX, posY), moveWayInDir (wayX, wayY) 1 value)
    | action == 'E' = ((posX, posY), moveWayInDir (wayX, wayY) 0 value)
    | action == 'W' = ((posX, posY), moveWayInDir (wayX, wayY) 2 value)
    | action == 'L' = ((posX, posY), rotateBy (wayX, wayY) (-value))
    | action == 'R' = ((posX, posY), rotateBy (wayX, wayY) value)
    | action == 'F' = ((posX + wayX * value, posY + wayY * value), (wayX, wayY))

main = do
    content <- readFile "input.txt"
    let actions = map getAction $ lines content
    let ((finalX, finalY), _) = foldl move ((0, 0), (10, 1)) actions
    print $ abs finalX + abs finalY
