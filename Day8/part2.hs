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

computeNext instructions index acc = (nextIns, possibleNextAcc) where
        nextIns = computeNextIns (instructions !! index) index
        possibleNextAcc = computeNextAcc (instructions !! index) acc

doOneStepModified instructions acc index = res where
        (ins, val) = instructions !! index
        res = case ins of
                "nop" -> (acc, index + val)
                "jmp" -> (acc, index + 1)
                otherwise -> (acc + val, index + 1)

doOneStep instructions acc index = res where
        (ins, val) = instructions !! index
        res = case ins of
                "nop" -> (acc, index + 1)
                "jmp" -> (acc, index + val)
                otherwise -> (acc + val, index + 1)

computeAcc instructions acc index visited changeAllowed
        | (index == length instructions) = case changeAllowed of
                True -> Nothing
                False -> Just acc 
        | (elem index visited) = Nothing
        | otherwise = res where
                (acc1, idx1) = doOneStep instructions acc index
                res1 = computeAcc instructions acc1 idx1 (index:visited) changeAllowed
                res = case res1 of
                        Just v -> Just v
                        Nothing -> case changeAllowed of
                                        False -> Nothing
                                        True -> computeAcc instructions acc2 idx2 (index:visited) False where
                                                (acc2, idx2) = doOneStepModified instructions acc index

main = do
    content <- readFile "input.txt"
    let instructions = map parseIns (lines content)
    let maybe_acc = computeAcc instructions 0 0 [] True
    putStrLn (case maybe_acc of
        Nothing -> "Not found"
        Just acc -> show acc)
