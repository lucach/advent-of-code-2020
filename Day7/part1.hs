import Data.List
import Data.Char
import qualified Data.Map as Map

canReachShinyGold _ "shinygold"  = True 
canReachShinyGold rulesMap color = or (map (canReachShinyGold rulesMap) colors) where
    colors = map snd (rulesMap Map.! color)

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
    let reachable = map (canReachShinyGold rulesMap) (Map.keys rulesMap)
    print ((length (filter (==True) reachable)) - 1)