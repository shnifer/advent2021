import Data.List

main :: IO ()
main = do
    raw <- readFile $ "input.txt"
    let dat = map read.lines $ raw :: [Int]
    print $ solve1 dat
    print $ solve2 dat


solve1 :: [Int] -> Int
solve1 xs = length $ filter (uncurry (>)) steps
    where steps = zip (tail xs) xs

solve2 :: [Int] -> Int
solve2 xs = solve1 windows
    where windows = map (sum.take 3) . filter ((>2).length) . tails $ xs