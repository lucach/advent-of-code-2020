strToInt [] _ = 0
strToInt (first: rest) symb0 = value + 2 * strToInt rest symb0 where
    value = if first == symb0 then 0 else 1

seatID seat = row * 8 + col where 
    (rowStr, colStr) = splitAt 7 seat
    [row, col] = map (\(str, symb0) -> strToInt (reverse str) symb0) (zip [rowStr, colStr] ['F', 'L'])

main = do
    content <- readFile "input.txt"
    let seatIDs = map seatID (lines content)
    print (maximum seatIDs)
