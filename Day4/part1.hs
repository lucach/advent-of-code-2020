import Data.List
import Data.List.Split

validPassport p = and [f p | f <- map isInfixOf ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]]

findPassports ls = splitOn "  " (intercalate " " ls)

main = do
    content <- readFile "input.txt"
    let passports = findPassports (lines content)
    let validPassports = map validPassport passports
    print (length (filter (==True) validPassports))