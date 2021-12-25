import qualified Data.Matrix as M
import qualified Data.Set as S
import Data.Set(Set, member, notMember)
import Control.Parallel.Strategies
import Data.Maybe

type V2 = (Int,Int)
data World = World {wx::Int, wy::Int, whs::Set V2, wvs::Set V2} deriving (Eq,Show)

main::IO ()
main = do
    dat <- getInput
    print $ solve1 dat

solve1::World -> Int
solve1 w = let
    iters = iterate step w
    chs = takeWhile (uncurry (/=)) $ iters `zip` tail iters
    in 1+ length chs

step::World -> World
step = moveV.moveH

moveH w = let
    hs = whs w
    free = S.filter (empty w.east w) hs
    moved = S.map (\v2 -> if v2 `member` free then east w v2 else v2) hs
    in w {whs = moved}

moveV w = let
    vs = wvs w
    free = S.filter (empty w.south w) vs
    moved = S.map (\v2 -> if v2 `member` free then south w v2 else v2) vs
    in w {wvs = moved}

empty::World -> V2 -> Bool
empty w v2 = v2 `notMember` (whs w) && v2 `notMember` (wvs w)
east::World -> V2 -> V2
east w (x,y) = if x+1 <= wx w then (x+1,y) else (0,y)
south::World -> V2 -> V2
south w (x,y) = if y+1 <= wy w then (x,y+1) else (x,0)

getTest1 = getFile "test1.txt"
getTest2 = getFile "test2.txt"
getInput = getFile "input.txt"
getFile fn = do
    raw <- lines <$> readFile fn
    let dat =  M.fromLists raw :: M.Matrix Char
    let ps = M.toList $ M.imap (\(r,c) v -> (c,r,v)) dat
    let hs = S.fromList $ mapMaybe (\(x,y,v)->if v == '>' then Just (x,y) else Nothing) ps
    let vs = S.fromList $ mapMaybe (\(x,y,v)->if v == 'v' then Just (x,y) else Nothing) ps
    pure $ World {wx = M.cols dat -1, wy = M.rows dat -1, whs = hs, wvs = vs}