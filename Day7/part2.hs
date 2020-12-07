import Data.List
import Data.Char
import qualified Data.Map as Map

count rulesMap color = sum directCounts + sum products where
    children = rulesMap Map.! color
    childrenCount = map (count rulesMap) (map snd children)
    directCounts = map fst children
    products = map (\(x,y) -> x*y) (zip childrenCount directCounts)

getRepr [num_str, col1, col2] = (read num_str :: Int, col1 ++ col2) 

processRule rule = (from_color, to_colors) where
    tokens = words rule
    from_color = concat (take 2 tokens)
    idxs = findIndices (\str -> isDigit (head str)) tokens
    to_colors = map getRepr (map (\id -> take 3 (drop id tokens)) idxs)

main = do
    content <- readFile "input.txt"
    let rules = lines content
    let rulesMap = Map.fromList (map processRule rules)
    print (count rulesMap "shinygold")