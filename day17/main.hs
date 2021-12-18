import Data.List

main::IO()
main = do
    print $ solve1
    print $ solve2

--solve1::Int
solve1 = let
    goodYL = head $ filter (findX.snd) $ map (\(vy, (l, _))->(vy,l)) goodYs
    in maximumY $ fst goodYL

solve2 = let
    goodYLs = [(y,l) | (y, (ml,d))<-goodYs, l<-[ml-(d-1)..ml]]
    vXYs = [(x,y) | (y,l) <- goodYLs, x <- findXs l]
    in length. nub $ vXYs

findX::Int -> Bool
findX l = not.null $ filter (>=minX) $ takeWhile (<=maxX) $ map (trajectX l) [1..maxX]

findXs::Int -> [Int]
findXs l = let
    vXXs = map (\x->(x,trajectX l x)) [1.. maxX] :: [(Int, Int)]
    goods = filter ((>=minX).snd) $ takeWhile ((<=maxX).snd) $ vXXs :: [(Int, Int)]
    in map fst goods

trajectX::Int->Int->Int
trajectX l sx = let
    traject = iterate (\(x,vx)->(x+vx, if vx>0 then vx-1 else 0)) (0,sx)
    in fst $ traject !! (l-1)

goodYs::[(Int, (Int, Int))]
goodYs = reverse $ map (\(v,(_,l))->(v,l)) $
         filter (fst.snd)$ map (\vy -> (vy, testY vy)) [minY..(-minY)]

testY::Int->(Bool, (Int, Int))
testY sy = let
    traject = takeWhile (>=minY) $ map fst $ trajectY sy
    good = filter (<=maxY) traject
    in (last traject<=maxY, (length traject, length good))

maximumY::Int->Int
maximumY sy = maximum $ takeWhile (>=0) $ map fst $ trajectY sy

trajectY::Int -> [(Int, Int)]
trajectY sy = iterate (\(y,vy) -> (y+vy,vy-1)) (0,sy)

--minX=20
--maxX=30
--minY=(-10)
--maxY=(-5)

minX=257
maxX=286
minY=(-101)
maxY=(-57)
