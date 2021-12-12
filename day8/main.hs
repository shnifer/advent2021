import Data.List.Split
import Data.List
import Data.Maybe

main::IO()
main = do
    raw <- lines <$> readFile "input.txt"
    print $ solve1 raw
    print $ solve2 raw

solve1::[String] -> Int
solve1 raw = let
    results = concatMap (snd.parseRaw) raw
    in length.filter(`elem` [2,3,4,7]).map length $ results

solve2::[String] -> Int
solve2 raw = sum . map calcNumber $ raw

calcNumber::String -> Int
calcNumber str = let
    (variants, results) = parseRaw str
    bce = determBCE variants
    digits = map (calcDigit bce) results
    in (digits!!0 * 1000 + digits!!1 * 100 + digits!!2 * 10 + digits!!3)

calcDigit::(Char,Char,Char) -> String -> Int
calcDigit (b,c,e) str =
    case (b `elem` str, c `elem` str , e `elem` str , length str) of
    (_, _, _, 2) -> 1
    (_, _, _, 3) -> 7
    (_, _, _, 4) -> 4
    (_, _, _, 7) -> 8
    (False, True, True, 5) -> 2
    (False, True, False, 5) -> 3
    (True, False, False, 5) -> 5
    (True, True, True, 6) -> 0
    (True, False, True, 6) -> 6
    (True, True, False, 6) -> 9
    otherwise -> error (show (b,c,e) ++ str)

determBCE::[String] -> (Char, Char, Char)
determBCE strs = let
    len56 = filter ((`elem` [5,6]).length) strs
    len5 = filter ((==5).length) len56
    digits56 = group . sort . concat $ len56
    digits5 = group . sort . concat $ len5
    e = head . fromJust $ find ((==3).length) digits56
    bc = map head $ filter ((==4).length) digits56
    c = head.filter (\d -> [d,d] `elem` digits5) $ bc
    b = head.filter (/=c) $ bc
    in (b,c,e)

parseRaw::String -> ([String], [String])
parseRaw str = (parts!!0, parts!!1) where
    parts = splitOn ["|"] . splitOn " " $ str