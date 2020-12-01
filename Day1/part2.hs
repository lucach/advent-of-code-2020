import Data.List

main = do
  content <- readFile "input.txt"
  let numbers = map read (lines content) :: [Int]
  let sums = [[n1,n2,n3] | (n1:rest) <- tails numbers, (n2:restrest) <- tails rest, n3 <- restrest]
  let maybe_ns = find (\ns -> sum ns == 2020) sums
  putStrLn (case maybe_ns of
    Nothing -> "Not found"
    Just ns -> show (product ns))
