{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), Map)
import Control.Parallel.Strategies
import Debug.Trace

data Op = INP | ADD | MUL | DIV | MOD | EQL deriving (Eq, Read, Show, Enum)
data Reg = W | X | Y | Z  deriving (Eq, Ord, Read, Show, Enum, Generic, NFData)
data NR = N Int | R Reg deriving (Eq, Show)
data Cmd = Cmd Op Reg NR deriving (Eq, Show)

data VM = VM {vmRs :: Map Reg Int, vmIn::[Int]} deriving (Eq, Show, Generic, NFData)

main::IO()
main = do
    dat <- getInput
    print $ solveWide dat
    print "Done"

params dat =let
    chs = chunksOf 18 dat
    ns i = map (\cs-> getCmdN $ cs!!i) chs
    in zip3 (ns 4) (ns 5) (ns 15)

solveWide dat = let
    pars = params dat
    in head.filter ((==0).fst) $ wideSteps $ take 14 pars

solveFast::[(Int,Int,Int)] -> [Int] -> Int
solveFast pars ins = foldl' (\z (w,p) -> stepFast z p w) 0 $ zip ins pars

wideStep::[(Int,[Int])] -> (Int,Int,Int) -> [(Int,[Int])]
wideStep vars p = let
    newVars = withStrategy (parListChunk 5000 rdeepseq) $ [ (stepFast z p w, w:way) | (z, way)<-vars, w<-[1..9]] :: [(Int,[Int])]
    grouped = groupBy (\a b -> fst a==fst b) $ sortOn fst newVars :: [[(Int,[Int])]]
    bests = for grouped $ \xs -> (fst.head $ xs , minimum $ map snd xs)
    in traceShow ((length.snd.head $ vars),(length vars)) $ bests

wideSteps pars = foldl' wideStep [(0,[])] pars

stepFast::Int -> (Int,Int,Int) -> Int -> Int
stepFast z0 (n1,n2,n3) w = let
    x = (z0 `mod` 26) + n2
    z = if x == w then
        (z0 `dv` n1)
        else
        26*(z0 `dv` n1) + (w+n3)
    in z

for xs f = map f xs

solve1::[Cmd] -> [Int]
solve1 cmds = let
    pars = params cmds
    f = solveFast pars
    results = map (\v -> (v, f v)) variants
    in fst. head $ withStrategy (parList rdeepseq )$ filter (\(_,z) -> z == 0) results

variants = sequence $ replicate 14 $ reverse [1..9]
--variants2 = filter (not.has0) [10^14-1, 10^14-2..10^13]

setReg::VM->Reg->Int->VM
setReg vm r v = vm{vmRs = M.insert r v $ vmRs vm}

getNR::VM->NR->Int
getNR _ (N x) = x
getNR vm (R r) = (vmRs vm) ! r

modifyR::VM->Reg->NR->(Int->Int->Int)->VM
modifyR vm r nr f = let
    v = getNR vm nr
    rv = getNR vm (R r)
    nv = rv `f` v
    in setReg vm r nv

execVM::VM -> [Cmd] -> VM
execVM vm cmds = foldl' execCmd vm cmds

startVM::[Int] -> VM
startVM ins = VM {vmRs = M.fromList [(W, 0), (X, 0), (Y, 0), (Z,0)], vmIn = ins}

execCmd::VM->Cmd->VM
execCmd vm (Cmd INP r _) = let
    (i:is) = vmIn vm
    in (setReg vm r i) {vmIn = is}
execCmd vm (Cmd ADD r nr) = modifyR vm r nr (+)
execCmd vm (Cmd MUL r nr) = modifyR vm r nr (*)
execCmd vm (Cmd DIV r nr) = modifyR vm r nr (dv)
execCmd vm (Cmd MOD r nr) = modifyR vm r nr (mod)
execCmd vm (Cmd EQL r nr) = modifyR vm r nr (\a b -> if a==b then 1 else 0)

dv a b = truncate $ fromIntegral a / fromIntegral b

parseCmd::String -> Cmd
parseCmd str = let
    strParts = splitOn " " str
    op = parseOp $ strParts !! 0
    r = fromR.parseNR $ strParts !! 1
    nr = if length strParts > 2 then parseNR $ strParts !! 2 else N 0
    in Cmd op r nr

parseOp::String -> Op
parseOp = read . map toUpper

parseNR::String -> NR
parseNR str = if isLetter.head $ str then R (read.map toUpper $ str) else N (read str)

fromR:: NR -> Reg
fromR (R r) = r

getCmdN (Cmd _ _ (N x)) = x

testVM = startVM [1,2,3,4,5,6,7,8,9,1,2,3,4,5]

getInput = getFromFile "input.txt"
getTest = getFromFile "test24.txt"

getFromFile::String -> IO [Cmd]
getFromFile fn = do
    raw <- lines <$> readFile fn
    pure $ map parseCmd raw