import Data.List.Extra
import Data.Function
import Data.Maybe
import Debug.Trace
import qualified Data.Set as S

type V3 = (Int,Int,Int)
type Part = [V3]
type SPart = S.Set V3
type Rot = Int

main::IO ()
main = do
    dat <- getInput
    let res = complete dat
    print $ solve1 res
    print $ solve2 dat res

manhL::V3->V3->Int
manhL (x1,y1,z1) (x2,y2,z2) = abs(x1-x2)+abs(y1-y2)+abs(z1-z2)

allPivots::[Part] -> [Part] -> [V3]
allPivots dat ps = map (uncurry findPivot) $ ps `zip` dat

findPivot::Part -> Part -> V3
findPivot a r = let
    rots = map snd $ allRots r :: [Part]
    mbDeltas = map (findDelta a) rots
    in fromJust $ firstJust id mbDeltas

findDelta::Part->Part->Maybe V3
findDelta a r = let
    ((xa,ya,za),(xr,yr,zr)) = (minimum a, minimum r)
    delta = (xr-xa, yr-ya, zr-za)
    moved = movePart a delta
    in if sort moved == sort r then Just delta else Nothing

solve2::[Part] -> [(SPart,Int)] -> Int
solve2 dat res = let
    ordPs = map (S.toList.fst) $ sortOn snd res
    pivots = allPivots dat ordPs
    allDist = [manhL a b | a<-pivots, b<-pivots]
    in maximum allDist

solve1::[(SPart,Int)] -> Int
solve1 = S.size.(S.unions).map fst


complete::[Part] -> [(SPart,Int)]
complete parts = let
    rotedParts = map (map snd.allRots) parts
    sRotedParts = (map.map) S.fromList rotedParts
    (ps,is) = completeStep sRotedParts [head.head $ sRotedParts] [0] 1
    in zip ps is

completeStep::[[SPart]] -> [SPart] -> [Int] -> Int -> ([SPart], [Int])
completeStep allRotParts rdyParts rdyInds frontSize = let
    pretInds = [0..length allRotParts -1 ] \\ rdyInds :: [Int]
    goods = nubOrdOn (fst) $ map (\(i,mv)->(i, fromJust mv)).filter (isJust.snd) $
            [ (pretInd, commons rdy rotPret) |
                rdy<-take frontSize rdyParts,
                pretInd<-pretInds,
                let rotPret = allRotParts!!pretInd] :: [(Int, SPart)]
    goodInds = map fst goods
    goodParts = map snd goods
    in trace ("added "++show goodInds) $ if null goods then (rdyParts, rdyInds)
       else completeStep allRotParts (goodParts ++ rdyParts ) (goodInds ++ rdyInds) (length goodInds)

commons::SPart -> [SPart] -> Maybe SPart
commons p1 rots = let
    matches = map (commonsMoved p1) rots :: [Maybe SPart]
    in firstJust id matches

commonsMoved::SPart -> SPart -> Maybe SPart
commonsMoved p1 p2 = let
    deltas = map (\((x1,y1,z1),(x2,y2,z2))-> (x1-x2,y1-y2,z1-z2)) $ S.toList $ S.cartesianProduct p1 p2
    variants = map (movePartS p2) deltas :: [SPart]
    intersects = map (S.size.(p1 `S.intersection`)) variants `zip` variants :: [(Int, SPart)]
    goods = filter ((>=12).fst) intersects
    in if null goods then Nothing else Just (snd.head $ goods)

movePartS::SPart -> V3 -> SPart
movePartS sp (dx,dy,dz) = S.map (\(x,y,z)->(x+dx,y+dy,z+dz)) sp

getInput = getFile "input.txt"
getTest = getFile "test.txt"

getFile::String -> IO [Part]
getFile fn = do
    raw <- lines <$> readFile fn
    let partsStrs = splitOn [""] raw
    pure $ map (\strs -> map parseV3 $ tail strs) partsStrs

parseV3::String -> V3
parseV3 str = let
    ps = map read . splitOn "," $ str :: [Int]
    in (ps!!0, ps!!1, ps!!2)

movePart::Part -> V3 -> Part
movePart p (dx,dy,dz) = map (\(x,y,z)->(x+dx,y+dy,z+dz)) p

allRots::Part -> [(Rot, Part)]
allRots p = map (\r->(r, rotPart p r)) [0..23]

rotPart::Part -> Rot -> Part
rotPart p n = map (rots n) p

rots::Rot -> (V3 -> V3)
rots n = [\(x,y,z)->(x,y,z),  \(x,y,z)->(y,-x,z), \(x,y,z)->(-x,-y,z), \(x,y,z)->(-y,x,z),
          \(x,y,z)->(z,y,-x), \(x,y,z)->(y,-z,-x),\(x,y,z)->(-z,-y,-x),\(x,y,z)->(-y,z,-x),
          \(x,y,z)->(-x,y,-z),\(x,y,z)->(y,x,-z), \(x,y,z)->(x,-y,-z), \(x,y,z)->(-y,-x,-z),
          \(x,y,z)->(-z,y,x), \(x,y,z)->(y,z,x),  \(x,y,z)->(z,-y,x),  \(x,y,z)->(-y,-z,x),
          \(x,y,z)->(x,z,-y), \(x,y,z)->(z,-x,-y),\(x,y,z)->(-x,-z,-y),\(x,y,z)->(-z,x,-y),
          \(x,y,z)->(-x,z,y), \(x,y,z)->(z,x,y),  \(x,y,z)->(x,-z,y),  \(x,y,z)->(-z,-x,y)]
            !! n
