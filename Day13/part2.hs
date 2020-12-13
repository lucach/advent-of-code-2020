import Data.List.Split

main = do
    content <- readFile "input.txt"
    let allIds = splitOn "," $ head $ tail $ lines content
    let ids = [(read id :: Integer, pos :: Integer) | (id, pos) <- zip allIds [0..], id /= "x"]
    -- Chinese Remainder Theorem
    let ms = map fst ids
    let as = map (\(a,b) -> a - b) ids
    let bs = map (div (product ms)) ms
    let binvs = map (\(b,m) -> (b ^ (m - 2)) `mod` m) $ zip bs ms  -- Euler's Theorem 
    let sol = (sum (zipWith3 (\a b c -> a * b * c) as bs binvs)) `mod` (product ms)
    print sol