import Data.List
import Data.List.Split

main::IO()
main = do
    raw <- readFile "input.txt"
    let dat = map read . splitOn "," $ raw :: [Int]
    print $ solve1 dat
    print $ solve2 dat

solve1::[Int] -> Int
solve1 dat = length $ steps !! 80 where
    steps = iterate step dat

step::[Int] -> [Int]
step dat = let
    ticked = map (\x -> if x>0 then x-1 else 6) dat
    newborn = length.filter (==0) $ dat
    in ticked++replicate newborn 8

solve2::[Int] -> Int
solve2 dat = let
 start = [length.filter(==v) $ dat | v<-[0..8]]
 steps = iterate step2 start
 in sum $ steps !! 256

step2::[Int] -> [Int]
step2 dat = let
    newborns = head dat
    aged = take 6 (tail dat) ++ [dat!!7+newborns, dat!!8, newborns]
    in aged