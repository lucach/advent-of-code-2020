strToInt [] _ = 0
strToInt (first: rest) symb0 = value + 2 * strToInt rest symb0 where
    value = if first == symb0 then 0 else 1

findMissing list = expectedSum - sum list where
    (min, max) = (minimum list, maximum list)
    expectedSum = (max * (max + 1) - (min - 1) * min) `div` 2

seatID seat = row * 8 + col where 
    (rowStr, colStr) = splitAt 7 seat
    [row, col] = map (\(str, symb0) -> strToInt (reverse str) symb0) (zip [rowStr, colStr] ['F', 'L'])

main = do
    content <- readFile "input.txt"
    let seatIDs = map seatID (lines content)
    print (findMissing seatIDs)
