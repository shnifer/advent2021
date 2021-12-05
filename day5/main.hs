import Data.List.Split
import Data.List

main::IO()
main = do
    raw <- lines <$> readFile "input.txt"
    let dat = parseRaw raw
    print $ solve line2points dat
    print $ solve line2points2 dat

solve::(Line->[Point]) -> [Line] -> Int
solve builder lins = let
    points = concatMap builder lins
    grouped = group.sort $ points
    in length.filter((>1).length) $ grouped

line2points::Line -> [Point]
line2points ((x1,y1), (x2,y2)) | x1 == x2 = [(x1,y) | y<-rng y1 y2]
line2points ((x1,y1), (x2,y2)) | y1 == y2 = [(x,y1) | x<-rng x1 x2]
line2points _ = []

line2points2::Line -> [Point]
line2points2 ((x1,y1), (x2,y2)) | x1 == x2 = [(x1,y) | y<-rng y1 y2]
line2points2 ((x1,y1), (x2,y2)) | y1 == y2 = [(x,y1) | x<-rng x1 x2]
line2points2 ((x1,y1), (x2,y2)) | x2-x1 == y2-y1 =
    [(x1+d, y1+d) | d<-rng 0 (x2-x1)]
line2points2 ((x1,y1), (x2,y2)) | x2-x1 == y1-y2 =
    [(x1+d, y1-d) | d<-rng 0 (x2-x1)]
line2points2 _ = []

rng a b = [min a b .. max a b]

type Point = (Int, Int)
type Line = (Point, Point)

parseRaw::[String]->[Line]
parseRaw raw = map parseLine raw

parseLine::String->Line
parseLine str = (parsePoint $ parts!!0, parsePoint $ parts!!1) where
 parts = splitOn " -> " str

parsePoint::String -> Point
parsePoint str = (read $ parts!!0, read $ parts!!1) where
                  parts = splitOn "," str