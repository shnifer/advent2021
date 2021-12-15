import qualified Data.Matrix as Mx
import Data.Matrix ((!), cols, rows, fromLists)
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M

main::IO ()
main = do
    m <- getInput
    print $ solve1 m
    print $ solve2 m

type IntM = Mx.Matrix Int
type V2 = (Int, Int)
type MapV2 = M.Map V2 Int

solve1 m = let res = floodM m [(0,0)] $ M.fromList [((0,0), 0)]
    in M.lookup ((rows m)-1, (cols m)-1) res

solve2 = solve1.augment5

floodM::IntM -> [V2] -> MapV2 -> MapV2
floodM _ [] m = m
floodM costs front m = let
    routeCost f t = fromJust((M.lookup) f m) + (costs ! t)
    routes = concatMap (\f -> map (\t->(t, routeCost f t)) (neighs costs f)) front
    groupedRoutes = groupBy (\a b -> fst a == fst b) . sortOn fst $ routes
    bests = map (head . sortOn snd) groupedRoutes
    newFronts = filter (\(t,c) ->
     case (M.lookup) t m of
        Nothing -> True
        Just cur -> c<cur
         ) bests
    newM = foldr (\(t,c) v->M.insert t c v) m newFronts
    in floodM costs (map fst newFronts) newM

neighs::IntM -> V2 -> [V2]
neighs m c = filter inBound $ coords4 c where
    coords4 (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    inBound (x,y) = (x>=0) && (x<rows m) && (y>=0) && (y<cols m)

augment5::IntM -> IntM
augment5 m = let
    s = rows m
    lists = Mx.toLists m
    longedLists = map longList lists
    moreLists = concat [ map (addList p) longedLists | p <- [0..4]]
    in fromLists moreLists

longList::[Int] -> [Int]
longList xs = concat [addList p xs | p<-[0..4]]
addList::Int -> [Int] -> [Int]
addList d xs = map (\v -> wrap (v+d)) xs

wrap::Int->Int
wrap x = if x>9 then x-9 else x

getInput::IO IntM
getInput = do
    raw <- lines <$> readFile "input.txt"
    let dat = map (map (\c-> read [c])) raw
    pure $ fromLists dat