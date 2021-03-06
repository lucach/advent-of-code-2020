import Data.List

preambleLength = 25
chunkLength = preambleLength + 1

findSequence ns = (init sequence, last sequence) where
    sequence = take chunkLength ns

findSequences ns
    | (length ns == chunkLength) = [findSequence ns]
    | otherwise = (findSequence ns : sequences) where
        sequences = findSequences (tail ns)

validSequence (ns, val) = valid where
    pairs = [[n1,n2] | (n1:rest) <- tails ns, n2 <- rest]
    maybe_pair = find (\p -> sum p == val) pairs
    valid = case maybe_pair of
        Nothing -> False
        Just p -> True

slice begin end = take (end - begin) . drop begin

findListWithSum magicValue ns begin end
    | currentSum < magicValue = findListWithSum magicValue ns begin (end + 1)
    | currentSum > magicValue = findListWithSum magicValue ns (begin + 1) end
    | currentSum == magicValue = slice begin end ns
    where currentSum = sum (slice begin end ns)

main = do
    content <- readFile "input.txt"
    let ns = map read (lines content) :: [Int]
    let sequences = findSequences ns
    let valids = map validSequence sequences
    putStrLn (case elemIndex False valids of
            Nothing -> "Not found!"
            Just idx -> show (minimum magicList + maximum magicList) where
                magicValue = ns !! (idx + preambleLength)
                magicList = findListWithSum magicValue ns 0 0)
