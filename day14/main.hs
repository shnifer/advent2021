import Data.List.Split
import Data.List
import qualified Data.Map as M
import Data.Maybe

main::IO ()
main = do
    (start, templates) <- getInput
    print (start, templates)
    print $ solve1 templates start 10
    print $ solve2 templates start 40

solve1::M.Map String Char -> String -> Int -> Int
solve1 tps start count = let
    result = (iterate (step tps) start) !! count
    counts = map length $ group.sort $ result
    in maximum counts - minimum counts

solve2 tps start count = let
    pairs = getPairs start `zip` repeat 1
    result = (iterate (step2 tps) pairs) !! count
    letters = (head start, 1) : map (\(pair, count) -> (pair!!1, count)) result
    groupedLetters = sumBySnd letters
    counts = map snd groupedLetters
    in maximum counts - minimum counts

step::M.Map String Char -> String -> String
step tps str = let
    augmented = map (augment tps) $ getPairs str
    in head str:(concatMap tail augmented)

getPairs::String -> [String]
getPairs str = take (length str -1 ) $ map (take 2) $ tails str

step2::M.Map String Char -> [(String,Int)] -> [(String,Int)]
step2 tps pairs = let
    augmentedPairs = concatMap (augmentPair tps) pairs :: [(String, Int)]
    wrapped = sumBySnd augmentedPairs
    in wrapped

sumBySnd:: Ord a => Num b=>[(a,b)] -> [(a,b)]
sumBySnd = map (\items -> (fst.head $ items, sum $ map snd items)).
           groupBy (\x y->fst x==fst y).
           sortOn fst

augmentPair::M.Map String Char -> (String, Int) -> [(String, Int)]
augmentPair tps
 (pair, count) = (\(a:b:c:_)->[([a,b], count), ([b,c], count)]) $ augment tps pair

augment:: M.Map String Char -> String -> String
augment tps pair@(a:b:_) = let
    c = fromJust $ M.lookup pair tps
    in [a,c,b]

getInput::IO (String, M.Map String Char)
getInput = do
    raw <- lines <$> readFile "input.txt"
    let splitedStrs = map (splitOn " -> ") $ drop 2 raw
    let pairs = map (\(x:y:_)->(x, head y)) splitedStrs
    pure (head raw, M.fromList pairs)