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
    location = read $ take (length locationStr - 3) $ drop 1 locationStr :: Integer
    value = read valueStr :: Integer

maskBit valueBit maskBit
    | maskBit == 'X' = valueBit
    | otherwise      = maskBit    

insertMasked location value memory mask = Map.insert location maskedVal memory where
    valStr = printf "%036b" value :: String
    maskedValStr = zipWith maskBit valStr mask
    maskedVal = toDec maskedValStr

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