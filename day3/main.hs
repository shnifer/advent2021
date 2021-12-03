import Data.List

main::IO ()
main = do
    raw <- lines <$> readFile "input.txt"
    print $ gammaRate raw * deltaRate raw
    print $ oxyRate raw * co2Rate raw

gammaRate::[String] -> Int
gammaRate = bitsToDecimal.popularBits

deltaRate::[String] -> Int
deltaRate = bitsToDecimal.inverseBits.popularBits

popularBits::[String] -> [Int]
popularBits strs = let
    hl = length strs `div` 2
    bitLists = transpose strs
    count1s = map (length.filter (=='1')) bitLists
    oftens = map (\l -> if l>hl then 1 else 0) count1s
    in oftens

bitsToDecimal::[Int] -> Int
bitsToDecimal bits = sum $ zipWith (\p b -> 2^p * b) [0..] (reverse bits)

inverseBits::[Int] -> [Int]
inverseBits = map (1-)

oxyCriteria::String -> Char
oxyCriteria xs = let
    c0 = length.filter(=='0') $ xs
    c1 = length xs - c0
    in if c1 >= c0 then '1' else '0'

co2Criteria::String -> Char
co2Criteria xs = let
    c0 = length.filter(=='0') $ xs
    c1 = length xs - c0
    in if c1 >= c0 then '0' else '1'

oxyRate::[String] -> Int
oxyRate strs = bitsToDecimal.strToBits.head $ slideFilter oxyCriteria 0 strs

co2Rate::[String] -> Int
co2Rate strs = bitsToDecimal.strToBits.head $ slideFilter co2Criteria 0 strs

slideFilter::(String -> Char) -> Int -> [String] -> [String]
slideFilter _ _ strs | length (strs) <= 1 = strs
slideFilter crit pos strs = let
    row = transpose strs !! pos
    target = crit row
    filtered = filter ((==target).(!!pos)) strs
    in slideFilter crit (pos+1) filtered

strToBits::String->[Int]
strToBits = map (\c -> if c == '0' then 0 else 1)