import Data.List.Split
import Data.List

main:: IO ()
main = do
    (points, folds) <- getInput
    print $ solve1 (points, folds)
    solve2 (points, folds)
    print "Done"

solve1::([V2], [Fold])->Int
solve1 (ps, folds) = length $ doFold ps (head folds)

solve2::([V2], [Fold])->IO [()]
solve2 (ps, folds) = mapM print $ strs
    where
     dots = foldl doFold ps folds
     mx = maximum $ map fst dots
     my = maximum $ map snd dots
     strs = map (\i -> composeStr i) [0..my]
     composeStr y = map (\x->if (x,y) `elem` dots then '*' else ',') [0..mx]


doFold::[V2] -> Fold -> [V2]
doFold ps f = nub $ map (foldP f) ps

foldP::Fold -> V2 -> V2
foldP (FX fx) (x,y) = (nx, y) where nx = if x<fx then x else 2*fx-x
foldP (FY fy) (x,y) = (x, ny) where ny = if y<fy then y else 2*fy-y

type V2 = (Int, Int)
data Fold = FX Int | FY Int deriving (Eq, Show)

getInput::IO ([V2], [Fold])
getInput = do
    raw <- splitOn [""]<$> lines <$> readFile "input.txt"
    let ps = map parseV2 (raw!!0)
    let folds = map parseFold (raw!!1)
    pure (ps, folds)

parseV2::String -> V2
parseV2 str = let parts = splitOn "," str in (read $ parts!!0, read $ parts!!1)

parseFold::String -> Fold
parseFold str | take 13 str == "fold along x=" = FX $ read (drop 13 str)
parseFold str | take 13 str == "fold along y=" = FY $ read (drop 13 str)