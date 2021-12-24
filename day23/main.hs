import qualified Data.Map as M
import Data.Map(Map, (!))
import Data.Maybe
import Debug.Trace
import Data.List
import Control.Parallel.Strategies

data Cell = A|B|C|D deriving (Eq, Show, Enum, Read)
cellToInt::Cell -> Int
cellToInt = (+1).fromEnum

data Pos = H Int | R Int Int deriving (Eq, Ord, Show, Read)
isH (H _) = True
isH _ = False
isR = not.isH

data World = World {wm::Map Pos Cell, wr::Map Int Int, wscore::Int} deriving (Eq,Show,Read)

maxroom = 4

main::IO()
main = do
    print $ solve2 0 inputW2

solve1::Int -> World ->Maybe Int
solve1 deep w = let
    fs = froms w ::[(Pos, Cell)]
    fts = [(f,t) | (f,c)<-fs, t<-tos w (f,c)] :: [(Pos,Pos)]
    ws = map (\(f,t)->move f t w) fts
    res = catMaybes $ withStrategy (parList rdeepseq) $ map (solve1 (deep+1)) ws
    in if isFinal w
        then Just $ wscore w
        else if deep>16 then Nothing
            else if null res then Nothing
                else Just $ minimum res

solve2::Int -> World ->Maybe Int
solve2 deep w = let
    fs = froms w ::[(Pos, Cell)]
    fts = [(f,t) | (f,c)<-fs, t<-tos w (f,c)] :: [(Pos,Pos)]
    ws = map (\(f,t)->move f t w) fts
    res = catMaybes $ withStrategy (parList rdeepseq) $ map (solve2 (deep+1)) ws
    in if isFinal w
        then Just $ wscore w
        else if deep>32 then Nothing
            else if null res then Nothing
                else Just $ minimum res

traceIf::Show a => Bool -> a -> b -> b
traceIf False _ b = b
traceIf True a b = traceShow a b

isFinal::World -> Bool
isFinal w = let
    ps = M.toList $ wm w
    in all isHome ps

isHome ((H _), c) = False
isHome ((R i _), c) = i == cellToInt c

move::Pos -> Pos -> World -> World
move from to w = let
    m = wm w
    c = m ! from
    m1 = M.delete from m
    m2 = M.insert to c m1
    r = wr w
    r1 = case from of
        H _ -> r
        R i _ -> M.adjust (+(-1)) i r
    r2 = case to of
        H _ -> r1
        R i _ -> M.adjust (+1) i r1
    d = dist from to
    score = d * price c
    in World {wm = m2, wr = r2, wscore = wscore w + score}

price::Cell -> Int
price c = 10^(fromEnum c)

tos::World -> (Pos, Cell) -> [Pos]
tos w (p@(H i), c) = let hp = homeP w c
    in if canHome w c && allowed w p hp then [hp] else []
tos w (p@(R i j), c) = let
    goodH = filter (allowed w p) allHPos
    hp@(R hi _) = homeP w c
    goodHp = if hi /= i && canHome w c && allowed w p hp then [hp] else []
    in if null goodHp then goodH else goodHp

allowed::World -> Pos -> Pos -> Bool
allowed w p1 p2 = let
    m = wm w
    hs = filter isH. map fst $ M.toList m
    (x1,_) = coords p1
    (x2,_) = coords p2
    rng = [min x1 x2 .. max x1 x2]
    in not.any (`elem` rng) $ map (fst.coords) (hs \\ [p1])

canHome::World -> Cell -> Bool
canHome w c = let
    i = cellToInt c
    n = wr w ! i
    in if n==maxroom then False
      else all (\j->(wm w) ! (R i j) == c) [1..n]

dist::Pos -> Pos -> Int
dist p1 p2 = let
    (x1,y1) = coords p1
    (x2,y2) = coords p2
    in if isR p1 && isR p2 then
        abs (x1-x2) + (maxroom+1-y1) + (maxroom+1-y2) else abs (x1-x2) + abs (y1-y2)

coords (H i) = (m!!(i-1),maxroom+1) where m = [1,2,4,6,8,10,11]
coords (R i y) = (m!!(i-1),y) where m = [3,5,7,9]

homeP::World -> Cell -> Pos
homeP w c = R i $ 1+(wr w) ! i
    where i = cellToInt c

froms::World -> [(Pos, Cell)]
froms w = let
    m = wm w
    hs = filter (isH.fst) $ M.toList m
    r = M.toList $ wr w
    rs = catMaybes $ map (\(i,c)-> let
        p = (R i c) in
        (\v->(p,v)) <$> M.lookup p m ) r
    nonHomeRs = filter (\(R i _,_)->not $ homeFilled w i) rs
    in hs++nonHomeRs

homeFilled::World -> Int -> Bool
homeFilled w i = let
    m = wm w
    n = wr w ! i
    c = toEnum (i-1) ::Cell
    in all (\j->M.lookup (R i j) m == Just c) [1..n]

allHPos = [H 1, H 2, H 3, H 4, H 5, H 6, H 7]
allPos = [H 1, H 2, H 3, H 4, H 5, H 6, H 7,
          R 1 1, R 1 2, R 2 1, R 2 2, R 3 1, R 3 2, R 4 1, R 4 2]

testW = World {wm =
     M.fromList [(R 1 2, B), (R 2 2, C), (R 3 2, B), (R 4 2, D),
                 (R 1 1, A), (R 2 1, D), (R 3 1, C), (R 4 1, A)],
     wr = M.fromList [(1,2),(2,2),(3,2),(4,2)],
     wscore = 0}

testW2 = World {wm =
     M.fromList [(R 1 4, B), (R 2 4, C), (R 3 4, B), (R 4 4, D),
                 (R 1 3, D), (R 2 3, C), (R 3 3, B), (R 4 3, A),
                 (R 1 2, D), (R 2 2, B), (R 3 2, A), (R 4 2, C),
                 (R 1 1, A), (R 2 1, D), (R 3 1, C), (R 4 1, A)],
     wr = M.fromList [(1,4),(2,4),(3,4),(4,4)],
     wscore = 0}

inputW = World {wm =
     M.fromList [(R 1 2, C), (R 2 2, A), (R 3 2, B), (R 4 2, C),
                 (R 1 1, D), (R 2 1, D), (R 3 1, B), (R 4 1, A)],
     wr = M.fromList [(1,2),(2,2),(3,2),(4,2)],
     wscore = 0}

inputW2 = World {wm =
     M.fromList [(R 1 4, C), (R 2 4, A), (R 3 4, B), (R 4 4, C),
                 (R 1 3, D), (R 2 3, C), (R 3 3, B), (R 4 3, A),
                 (R 1 2, D), (R 2 2, B), (R 3 2, A), (R 4 2, C),
                 (R 1 1, D), (R 2 1, D), (R 3 1, B), (R 4 1, A)],
     wr = M.fromList [(1,4),(2,4),(3,4),(4,4)],
     wscore = 0}

-- #############
-- #12.3.4.5.67#
-- ### # # # ### R x 2
--   # # # # #   R x 1
--   #########
--   R0 R1