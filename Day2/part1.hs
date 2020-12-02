import Data.List.Split

main = do
    content <- readFile "input.txt"
    let rows = map words (lines content)
    let passwords = [(head c,pwd) | [_, c, pwd] <- rows]
    let minmaxs = map (\r -> splitOn "-" (head r)) rows
    let minmaxs_int = map (map read) minmaxs
    let lengths = [length (filter (== c) pwd) | (c, pwd) <- passwords]
    let valids = [l >= min && l <= max | ([min, max], l) <- zip minmaxs_int lengths]
    print (length (filter (== True) valids))
