countTrees rows cols (slope_col, slope_row) = length (filter (== '#') chars) where
    used_rows = [0,slope_row..(length rows - 1)]
    indexes = [mod (x*slope_col) cols | x <- [0..(length used_rows - 1)]]
    chars = [(rows !! row_idx) !! index | (row_idx, index) <- zip used_rows indexes]

main = do
    content <- readFile "input.txt"
    let rows = lines content
    let cols = length (head rows)
    let trees = map (countTrees rows cols) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print (product trees)
