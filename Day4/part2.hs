import Data.List
import Data.List.Split
import Data.Char

getValue fields key = case find (\[k, v] -> k == key) fields of
    Nothing -> Nothing
    Just [k,v] -> Just v

extractAndCheck check_fn maybe_value = case maybe_value of
    Nothing -> False
    Just value -> check_fn value

between lower upper n_str = n >= lower && n <= upper where
    n = read n_str

isIn list_strs str = or (map (== str) list_strs)

checkLength l str = (length str) == l
    
checkAllChars check_fn str = all check_fn str

checkFirst c str = (head str) == c

checkAllCharsFromSecond check_fn str = checkAllChars check_fn (tail str)

checkHeight str = variant1 || variant2 where
    (v_str,sym) = splitAt ((length str) - 2) str
    variant1 = sym == "cm" && between 150 193 v_str
    variant2 = sym == "in" && between 59 76 v_str

validPassport p = and [eclCheck, byrCheck, iyrCheck, eyrCheck, pidCheck, hclCheck, hgtCheck] where
    fields = map (splitOn ":") (splitOn " " p)
    [ecl, byr, iyr, eyr, pid, hcl, hgt] = map (getValue fields) ["ecl", "byr", "iyr", "eyr", "pid", "hcl", "hgt"]
    eclCheck = extractAndCheck (isIn ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) ecl
    byrCheck = extractAndCheck (between 1920 2002) byr
    iyrCheck = extractAndCheck (between 2010 2020) iyr
    eyrCheck = extractAndCheck (between 2020 2030) eyr
    pidCheck = extractAndCheck (checkLength 9) pid && extractAndCheck (checkAllChars isDigit) pid
    hclCheck = extractAndCheck (checkFirst '#') hcl && (extractAndCheck (checkAllCharsFromSecond isHexDigit) hcl)
    hgtCheck = extractAndCheck checkHeight hgt

findPassports ls = splitOn "  " (intercalate " " ls)

main = do
    content <- readFile "input.txt"
    let passports = findPassports (lines content)
    let validPassports = map validPassport passports
    print (length (filter (==True) validPassports))