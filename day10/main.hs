import Data.Maybe
import Data.List

main::IO ()
main = do
    raw <- lines <$> readFile "input.txt"
    print $ solve1 raw
    print $ solve2 raw

solve1::[String]->Int
solve1 raw = let
    results = map (parse "") raw
    corrupts = filter (isJust.snd) results
    price ')' = 3
    price ']' = 57
    price '}' = 1197
    price '>' = 25137
    in sum.map (price.fromJust.snd) $  corrupts

solve2 raw = let
    results = map (parse "") raw
    incomplete = map fst $ filter (\(stack, err) -> not (null stack) && err == Nothing ) results
    scores = map completeScore incomplete
    midI = length scores `div` 2
    in (sort scores) !! midI


completeScore::String -> Int
completeScore = foldl (\s c -> s*5 + score c) 0 where
    score '(' = 1
    score '[' = 2
    score '{' = 3
    score '<' = 4


parse::String -> String -> (String, Maybe Char)
parse stack [] = (stack, Nothing)
parse stack (x:xs) | isOpen x = parse (x:stack) xs
parse stack (x:xs) | not (null stack) && x `closes` head stack = parse (tail stack) xs
parse stack (x:_) = (stack, Just x)

closes::Char->Char->Bool
closes ']' '[' = True
closes '>' '<' = True
closes ')' '(' = True
closes '}' '{' = True
closes _ _ = False

isOpen::Char->Bool
isOpen c = c `elem` "[<({"