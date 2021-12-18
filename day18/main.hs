import Data.Char
import Data.List
import Data.Maybe

data Value = N Int | P Value Value deriving (Eq)
data Way = L | R deriving (Eq, Ord, Show)
type Route = [Way]

main :: IO()
main = do
    raw <- lines <$> readFile "input.txt"
    let dat = map parse raw
    print $ solve1 dat
    print $ solve2 dat

solve1::[Value] -> Int
solve1 vs = magnitude $ foldl1 add vs

solve2 vs = let
    ps = [(a,b) | a <- vs, b <- vs, a/=b]
    ms = map (\(a,b) -> magnitude $ add a b) ps
    in maximum ms

magnitude::Value -> Int
magnitude (N x) = x
magnitude (P v1 v2) = 3*magnitude v1 + 2*magnitude v2

add::Value -> Value -> Value
add v1 v2 = reduce $ P v1 v2

allRoutes::Value -> [Route]
allRoutes (N _) = [[]]
allRoutes (P v1 v2) = map (L:) (allRoutes v1)  ++ map (R:) (allRoutes v2)

pairRoutes::[Route] -> [Route]
pairRoutes rs = nub $ filter (not.null) $ map init $ rs

reduce::Value -> Value
reduce v = let
    vs = iterate reduceStep v
    z = zip vs $ tail vs
    in fst $ head $ filter (uncurry (==)) z

reduceStep::Value -> Value
reduceStep v = let
    ar = allRoutes v
    pr = pairRoutes ar
    mbr = findBangPair pr
    msr = findSplitRoute v ar
    in case mbr of
      Just br -> doBang v ar br
      Nothing -> case msr of
        Nothing -> v
        Just sr -> modSplit v sr

doBang:: Value -> [Route] -> Route -> Value
doBang v ar br = let
    mlr = lFromPair ar br
    mrr = rFromPair ar br
    (nl, nr) = getPair v br
    v1 = maybe v (modAdd nl v) mlr
    v2 = maybe v1 (modAdd nr v1) mrr
    v3 = modSet0 v2 br
    in v3

modSet0 :: Value -> Route -> Value
modSet0 = modify (const $ N 0)

modAdd::Int -> Value -> Route -> Value
modAdd d  = modify (\(N x) -> N $ x+d)

modSplit:: Value -> Route -> Value
modSplit = modify (\(N x) -> P (N (x `div` 2)) (N (x `div` 2 + x `mod` 2)))

modify:: (Value -> Value) -> Value -> Route -> Value
modify f v [] = f v
modify f (P v1 v2) (L:t) = P (modify f v1 t) v2
modify f (P v1 v2) (R:t) = P v1 (modify f v2 t)

getPair::Value -> Route -> (Int, Int)
getPair (P (N a) (N b)) [] = (a,b)
getPair (P v1 _) (L:t) = getPair v1 t
getPair (P _ v2) (R:t) = getPair v2 t

getN::Value -> Route -> Int
getN (N x) [] = x
getN (P v1 _) (L:t) = getN v1 t
getN (P _ v2) (R:t) = getN v2 t

--from all routes
findSplitRoute :: Value -> [Route] -> Maybe Route
findSplitRoute v ar = listToMaybe $ filter (\r -> getN v r >= 10) ar

-- from pairs
findBangPair::[Route] -> Maybe Route
findBangPair routes = listToMaybe $ filter (\r -> length r >= 4) routes

lFromPair:: [Route] -> Route -> Maybe Route
lFromPair ar r = listToMaybe.reverse $ filter (<r) ar

rFromPair:: [Route] -> Route -> Maybe Route
rFromPair ar r = listToMaybe $ filter (\v -> v>r && take (length r) v /= r) ar

parse::String -> Value
parse str | isDigit $ head str = N (read str)
parse str | head str == '[' = P (parse p1) (parse p2) where
    pos = findSeparator 0 0 str
    p1 = tail $ take pos str
    p2 = init $ drop (pos+1) str

findSeparator::Int -> Int -> String -> Int
findSeparator pos 1 (',':t) = pos
findSeparator pos depth ('[':t) = findSeparator (pos+1) (depth+1) t
findSeparator pos depth (']':t) = findSeparator (pos+1) (depth-1) t
findSeparator pos depth str = findSeparator (pos+1) depth (tail str)

instance Show (Value) where
    show (N n) = show n
    show (P v1 v2) = "["++(show v1)++","++(show v2)++"]"