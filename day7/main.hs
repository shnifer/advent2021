import Data.List.Split

main::IO()
main = do
    raw<- readFile "input.txt"
    let dat = read <$> splitOn "," raw :: [Int]
    print $ solve1 dat
    print $ solve2 dat

solve1::[Int]->Int
solve1 xs = let
    variants = [minimum xs .. maximum xs] :: [Int]
    results = map (\p -> sum $ map (abs.(p-)) xs) variants :: [Int]
    in minimum results

solve2::[Int]->Int
solve2 xs = let
    variants = [minimum xs .. maximum xs] :: [Int]
    results = map (\p -> sum $ map (cost p) xs) variants :: [Int]
    in minimum results
    where cost a b = let n = abs (a-b) in n*(n+1) `div` 2