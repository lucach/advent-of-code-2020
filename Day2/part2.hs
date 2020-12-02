import Data.List.Split
import Data.Bits

main = do
    content <- readFile "input.txt"
    let rows = map words (lines content)
    let letters = [head c | [_, c, _] <- rows]
    let passwords = [pwd | [_, _, pwd] <- rows]
    let positions = map (\r -> splitOn "-" (head r)) rows
    let positions_int = map (map ((subtract 1) . read)) positions
    let chars = map (\([pos1, pos2], pwd) -> (pwd !! pos1, pwd !! pos2)) (zip positions_int passwords)
    let valids = [xor (fst c == letter) (snd c == letter) | (c, letter) <- zip chars letters]
    print (length (filter (== True) valids))
