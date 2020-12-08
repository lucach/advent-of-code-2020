import qualified Data.Text as T
import Data.Text.Read

parseIns insStr = (ins, val) where
    [ins, valStr] = words insStr
    val = case signed decimal (T.pack valStr) of
        Left err -> 0
        Right (val, _) -> val

computeNextIns (ins, value) index = case ins of
        "jmp" -> index + value
        otherwise -> index + 1

computeNextAcc (ins, value) acc = case ins of
        "acc" -> acc + value
        otherwise -> acc

computeAcc instructions acc index visited = nextAcc where
    nextIns = computeNextIns (instructions !! index) index
    possibleNextAcc = computeNextAcc (instructions !! index) acc
    nextAcc = case elem index visited of
        True -> acc
        False -> computeAcc instructions possibleNextAcc nextIns (index:visited)

main = do
    content <- readFile "input.txt"
    let instructions = map parseIns (lines content)
    print (computeAcc instructions 0 0 [])