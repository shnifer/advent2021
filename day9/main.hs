import qualified Data.Matrix as M
import Data.Matrix ((!))
import Data.List

main::IO()
main = do
    m <- getInput
    print $ solve1 m
    print $ solve2 m

solve1::M.Matrix Int -> Int
solve1 m = sum $ map ((+1).(m!)) $ findVents m

solve2::M.Matrix Int -> Int
solve2 m = let
    vents = findVents m
    basins = map (basin m) vents
    lens = map length basins
    in product . take 3 . reverse . sort $ lens

basin::M.Matrix Int -> (Int,Int) -> [(Int,Int)]
basin m c = spread m [] [c]

spread::M.Matrix Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
spread m olds fronts = let
    pretends = nub.concatMap (goodCoords m) $ fronts
    newFronts = filter (\c->(m!c)<9 && not (c `elem` olds)) pretends
    in if null pretends then olds++fronts
    else spread m (olds++fronts) newFronts

findVents::M.Matrix Int -> [(Int, Int)]
findVents m = filter inVent $ allCoords m where
    inVent c = let
        v = m ! c
        ns = neighs m c
        in all (>v) ns

neighs::M.Matrix Int -> (Int, Int) -> [Int]
neighs m c = map (m !) $ goodCoords m c

goodCoords::M.Matrix Int -> (Int, Int) -> [(Int, Int)]
goodCoords m c = filter inBound $ coords4 c where
    coords4 (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    inBound (x,y) = (x>=0) && (x<M.rows m) && (y>=0) && (y<M.cols m)

allCoords::M.Matrix Int -> [(Int,Int)]
allCoords m = [(r,c) | r<-[0..(M.rows m)-1], c<-[0..(M.cols m)-1]]

getInput::IO (M.Matrix Int)
getInput = do
    raw <- lines <$> readFile "input.txt"
    let dat = map (map (\x -> read [x])) raw ::[[Int]]
    pure $ M.fromLists dat
