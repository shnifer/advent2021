import qualified Data.Vector as V
import Data.Vector(Vector, (!))
import qualified Data.Set as S
import Data.Set(Set, member, size)
import Data.List.Split
import Data.Maybe
import Data.Function
import Control.Parallel.Strategies

type V2 = (Int, Int)
type Mx = Set V2
type Vx = Vector Bool
type XXYYRect = (Int, Int, Int, Int)

main::IO()
main = do
    (v,m) <- getInput
    print $ solve1 v m
    print $ solve2 v m

solve1::Vx -> Mx -> Int
solve1 v m = let
    rect = expandRect 3 $ mxRect m :: XXYYRect
    res = (iterate (step v rect)  m) !! 2
    in size res

solve2 v m = let
    rect = expandRect 50 $ mxRect m
    res = (iterate (step v rect) m) !! 50
    in size res

step::Vx -> XXYYRect -> Mx -> Mx
step vx rect@(x1,_,y1,_) mx = let
    bv = (x1,y1) `member` mx
    newDots = withStrategy (parListChunk 200 rdeepseq) $
        map (\v->(calcDot vx mx rect bv v,v)) $ allDots rect :: [(Bool,V2)]
    in S.fromList $ map snd $ filter fst newDots

calcDot::Vx -> Mx -> XXYYRect -> Bool -> V2 -> Bool
calcDot vs mx r bv v2 = let
    n = getN mx v2 r bv
    in vs ! n

getN::Mx -> V2 -> XXYYRect ->Bool -> Int
getN mx (x,y) r@(x1,_,y1,_) bv = let
    coords = [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x,y), (x+1,y),(x-1,y+1), (x,y+1), (x+1,y+1)]
    bits = map (\v -> if v `inRect` r then v `member` mx else bv) coords
    in foldl (\v d -> if d then v*2+1 else v*2) 0 bits

expandRect::Int -> XXYYRect -> XXYYRect
expandRect d (x1,x2,y1,y2) = (x1-d,x2+d,y1-d,y2+d)

inRect::V2 -> XXYYRect -> Bool
inRect (x,y) (x1,x2,y1,y2) = x `elem` [x1..x2] && y `elem` [y1..y2]

allDots::XXYYRect -> [V2]
allDots (x1,x2,y1,y2) = [(x,y) | y<-[y1..y2], x<-[x1..x2]]

mxRect :: Mx -> XXYYRect
mxRect m = let
    (xs,ys) = (S.map fst m ,S.map snd m)
    in (fromJust $ S.lookupMin xs, fromJust $ S.lookupMax xs,
        fromJust $ S.lookupMin ys, fromJust $ S.lookupMax ys)

getInput = getFile "input.txt"
getTest = getFile "test.txt"

getFile::String -> IO (Vx, Mx)
getFile fn = do
    raw <- lines <$> readFile fn
    let partsStrs = splitOn [""] raw
    let vx = V.fromList $ map (=='#') $ partsStrs!!0!!0 ::Vx
    let strs = partsStrs !! 1
    let dots = [(x,y) | y<-[0..length strs - 1], x<-[0..length (strs!!0) -1], strs!!y!!x == '#']
    let mx = S.fromList dots:: Mx
    pure $ (vx, mx)