import Data.Maybe
import Data.List
import Data.List.Split
import Control.Parallel.Strategies

main::IO()
main = do
    dat <- getInput
    print $ solve1 dat
    print $ solve2 dat

solve1 dat = let
    result = foldl' step [] dat
    trimmed = catMaybes $ map (cubeIntersect trim) result
    in sum.map size $ trimmed
        where trim = ((-50,50),(-50,50),(-50,50))

solve2 dat = length $ map size result
    where result = foldl' step [] dat

step::[Cube] -> Act -> [Cube]
step cubes (True, cube) = cube : concatMap (exclude cube) cubes
step cubes (False, cube) = concatMap (exclude cube) cubes

exclude::Cube -> Cube -> [Cube]
exclude tool cube = let
    msect = cubeIntersect tool cube
    in case msect of
        Nothing -> [cube]
        Just isect | isect == cube -> []
        Just isect | otherwise -> let
            sects = sect6 isect
            in catMaybes $ map (cubeIntersect cube) sects

size::Cube -> Int
size ((x1,x2),(y1,y2),(z1,z2)) = (x2-x1+1)*(y2-y1+1)*(z2-z1+1)

sect6::Cube -> [Cube]
sect6 (xr@(x1,x2),yr@(y1,y2),zr@(z1,z2)) =
    [ (rInf         , rInf        , (mInf, z1-1))
    , (rInf         , rInf        , (z2+1, pInf))
    , ((mInf, x1-1) , rInf         , zr)
    , ((x2+1, pInf) , rInf         , zr)
    , (xr           , (mInf, y1-1) , zr)
    , (xr           , (y2+1, pInf) , zr) ]
    where mInf = minBound::Int
          pInf = maxBound::Int
          rInf = (mInf,pInf)

type Act = (Bool, Cube)
type Cube = (Range,Range,Range)
type Range = (Int,Int)
type V3 = (Int,Int,Int)

parseRaw::String -> Act
parseRaw str = let
    parts = splitOn " " str
    on = parts!!0 == "on"
    coordPart = map (drop 2) $ splitOn "," (parts!!1)
    ranges = map parseRange coordPart
    in (on, (ranges!!0, ranges!!1, ranges!!2))
    where
        parseRange ::String -> (Int,Int)
        parseRange str = let
            parts = map read $ splitOn ".." str :: [Int]
            in (parts!!0, parts!!1)

cubeIntersect::Cube -> Cube -> Maybe Cube
cubeIntersect (xr1,yr1,zr1) (xr2,yr2,zr2) = do
    xr <- rngIntersect xr1 xr2
    yr <- rngIntersect yr1 yr2
    zr <- rngIntersect zr1 zr2
    pure $ (xr,yr,zr)

rngIntersect::Range -> Range -> Maybe Range
rngIntersect (a1,b1) (a2,b2) = let
    a = max a1 a2
    b = min b1 b2
    in if a<=b then Just (a,b) else Nothing

cubeContains::Cube -> Cube -> Bool
cubeContains (xr1,yr1,zr1) (xr2,yr2,zr2) =
    and [xr1 `rngContains` xr2, yr1 `rngContains` yr2, zr1 `rngContains` zr2]

rngContains::Range -> Range -> Bool
rngContains (a1,b1) (a2,b2) =
    and [a2>=a1, a2<=b1, b2>=a1, b2<=b1]

getTest = getFile "test.txt"
getTest2 = getFile "test2.txt"
getInput = getFile "input.txt"

getFile::String -> IO [Act]
getFile fn = do
    raw <- lines <$> readFile fn
    pure $ map parseRaw raw
