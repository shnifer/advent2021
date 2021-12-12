import qualified Data.Matrix as M
import Data.Matrix ((!))
import qualified Data.Set as S
import Data.Set ((\\), member, union)

main::IO()
main = do
    m <- getInput
    print $ solve1 m
    print $ solve2 m

solve1::M.Matrix Int -> Int
solve1 m = sum.take 100.map snd.tail $ iterate (step.fst) (m,0)

solve2::M.Matrix Int -> Int
solve2 m = let
    results = map fst $ iterate (step.fst) (m,0)
    sums = map (M.foldl (+) 0) results
    indexes = filter (\i -> sums!!i == 0) [0..]
    in head indexes

step::M.Matrix Int -> (M.Matrix Int, Int)
step m = let
    increased = M.map (+1) m
    (newM, flashes) = calcFlashed increased (allMCoords m) S.empty
    toRest = M.map (\v->if v>9 then 0 else v) newM
  in (toRest, length flashes)

calcFlashed::M.Matrix Int -> S.Set (Int,Int) -> S.Set (Int,Int) -> (M.Matrix Int, S.Set (Int,Int))
calcFlashed m front used | null front = (m, used)
calcFlashed m front used = let
    flashes = (S.filter ((>9).(m !)) front) \\ used
    newUsed = used `union` flashes
    enlighted = (S.unions $ S.map (n8 m) flashes) \\ newUsed
    newM = M.imap (\c v-> v+ (S.size $ (n8 m c) `S.intersection` flashes)) m
    in calcFlashed newM enlighted newUsed


allMCoords::M.Matrix Int -> S.Set (Int,Int)
allMCoords m = S.fromList [(r,c) | r<-[0..M.rows m -1] , c<-[0..M.cols m -1]]

n8::M.Matrix Int -> (Int,Int) -> S.Set(Int,Int)
n8 m (x,y) = S.filter (\(x,y)->x>=0 && y>=0 && x<M.rows m && y<M.cols m) $
    S.fromList [(x+dx,y+dy) | dx<-[(-1)..1], dy<-[(-1)..1]]

getInput::IO (M.Matrix Int)
getInput = do
    raw <- lines <$> readFile "input.txt"
    let dat = map (map (\x -> read [x])) raw ::[[Int]]
    pure $ M.fromLists dat