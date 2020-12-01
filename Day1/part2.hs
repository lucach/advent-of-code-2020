import Data.List

main = do
  content <- readFile "input.txt"
  let ns = map read (lines content) :: [Int]
  let pairs = [[n1,n2,n3] | (n1:rest) <- tails ns, (n2:restrest) <- tails rest, n3 <- restrest]
  let maybe_pair = find (\p -> sum p == 2020) pairs
  putStrLn (case maybe_pair of
    Nothing -> "Not found"
    Just pair -> show (product pair))
