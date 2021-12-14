import qualified Data.Map as M
import Data.List.Split
import Data.List
import Data.Char
import Debug.Trace

main::IO()
main = do
    raw <- lines <$> readFile "input.txt"
    let g = parseRaw raw
    print $ solve1 g
    print $ solve2 g

solve1::Gr -> Int
solve1 gr = calcWays gr "start" ["start"]

solve2::Gr -> Int
solve2 gr = calcWays2 gr "start" [] False

calcWays::Gr -> String -> [String] -> Int
calcWays _ "end" _ = 1
calcWays gr start used = let
    variants = filter (not.(`elem` used)) $ (M.!) gr start
    newUsed p = if (isUpper $ head p) then used else (p:used)
    in sum $ map (\p -> calcWays gr p (newUsed p)) variants

calcWays2::Gr -> String -> [String] -> Bool -> Int
calcWays2 _ "end" _ _ = 1
calcWays2 gr start used doubleUsed = let
    allVariants = filter (/="start") $ (M.!) gr start
    variants = if doubleUsed then filter (not.(`elem` used)) allVariants else allVariants
    newUsed p = if (isUpper $ head p) then used else (p:used)
    newDoubleUsed p = p `elem` used || doubleUsed
    in sum $ map (\p -> calcWays2 gr p (newUsed p) (newDoubleUsed p)) variants

type Gr = M.Map String [String]

parseRaw::[String] -> Gr
parseRaw raw = let
    paths = map (splitOn "-") raw
    ways = concatMap (\(from:to:_) -> [(from,to), (to, from)]) paths :: [(String, String)]
    points = nub.map fst $ ways :: [String]
    ns p = map snd $ filter ((==p).fst) ways
    grouped = map (\p->(p,ns p)) points :: [(String, [String])]
    in M.fromList grouped