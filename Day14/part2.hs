import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Text.Printf
import Data.Char (digitToInt)

toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

parseLine line
    | "mask" `isPrefixOf` line = ("mask", drop 7 line)
    | otherwise = ("mem", drop 3 line)

parseMemIns ins = (location, value) where
    [locationStr, valueStr] = splitOn "=" ins
    location = read $ take (length locationStr - 3) $ drop 1 locationStr :: Int
    value = read valueStr :: Int

maskBit valueBit maskBit
    | maskBit == '0' = valueBit
    | otherwise      = maskBit

getAllLocations :: String -> Int -> String -> [Int]
getAllLocations str idx builtStr = case idx of
    36 -> [toDec builtStr]
    _ -> case str !! idx of
        'X' -> getAllLocations str (idx + 1) (builtStr ++ "0") ++ getAllLocations str (idx + 1) (builtStr ++ "1")
        _ -> getAllLocations str (idx + 1) (builtStr ++ [str !! idx])


insertMasked location value memory mask = foldl (\map loc -> Map.insert loc value map) memory locations where
    locationStr = printf "%036b" location :: String
    maskedLocationStr = zipWith maskBit locationStr mask
    locations = getAllLocations maskedLocationStr 0 ""

executeInstruction (memory, mask) instruction = (newMemory, newMask) where
    newMask = case fst instruction of
        "mask" -> snd instruction
        _ -> mask
    newMemory = case fst instruction of
        "mask" -> memory
        _ -> insertMasked location value memory mask where
            (location, value) = parseMemIns $ snd instruction

main = do
    content <- readFile "input.txt"
    let instructions = map parseLine $ lines content
    let (finalMemory, finalMask) = foldl executeInstruction (Map.empty, "") instructions
    print $ sum $ Map.elems finalMemory