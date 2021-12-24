import Control.Monad.State.Strict
import Debug.Trace
import Data.List
import Control.Parallel.Strategies

main::IO()
main = print $ uncurry max $ games2 inputWorld2

data Player = Player {playerPos::Int, playerScore::Int} deriving (Show)
data Dice = Dice {dicePos::Int, diceRolls::Int} deriving (Show)
data World = World {turnN::Int, wp1::Player, wp2::Player, wDice::Dice} deriving (Show)
type ST = State World

data World2 = World2 {turnN2::Int, p1::Player, p2::Player} deriving (Show)
type ST2 = State World2

games2::World2 -> (Int,Int)
games2 w | playerScore (p1 w) >=21 = (1,0)
games2 w | playerScore (p2 w) >=21 = (0,1)
games2 w = let
    nws = map (\(d,c) ->(turn2 d w, c)) qd :: [(World2,Int)]
    nrs = map (\(w,c)-> let (a,b) = games2 w in (c*a,c*b)) nws :: [(Int,Int)]
    par = nrs `using` parList rdeepseq
    in sumPairs par

qd::[(Int,Int)]
qd = let
    rs = sort [x+y+z | x<-[1..3], y<-[1..3], z<-[1..3]]
    in map (\gr -> (head gr, length gr)) $ group rs

sumPairs::[(Int,Int)]->(Int,Int)
sumPairs ps = ( sum $ map fst ps, sum $ map snd ps)

turn2::Int->World2->World2
turn2 die w = let
    pn = turnN2 w `mod` 2
    w1 = case pn of
        1 -> w{p1 = playerMove die $ p1 w}
        0 -> w{p2 = playerMove die $ p2 w}
    in w1{turnN2 = 1+ turnN2 w1}

runGame::ST ()
runGame = do
    w <- get
    case hasWinner w of
        True -> pure ()
        False -> do
            turn
            runGame

turn::ST ()
turn = do
    pn <- (`mod` 2) <$> gets turnN
    move <- get3dice
    case pn of
        1 -> modify (\w -> w{wp1 = playerMove move $ wp1 w})
        0 -> modify (\w -> w{wp2 = playerMove move $ wp2 w})
    modify $ (\w->w{turnN = 1+ turnN w})

testWorld::World
testWorld = World {turnN=1, wp1= Player 4 0, wp2=Player 8 0, wDice = Dice 1 0}

testWorld2 = World2 {turnN2=1, p1= Player 4 0, p2=Player 8 0}
inputWorld2 = World2 {turnN2=1, p1= Player 3 0, p2=Player 5 0}

inputWorld::World
inputWorld = World {turnN=1, wp1= Player 3 0, wp2=Player 5 0, wDice = Dice 1 0}

hasWinner::World -> Bool
hasWinner w = (playerScore $ wp1 w) >=1000 ||  (playerScore $ wp2 w)>=1000

get3dice::ST Int
get3dice = do
    d <- dicePos <$> gets wDice
    modify $ (\w -> w {wDice = addRolls 3 $ wDice w})
    pure $ d + (d+1) + (d+2)

addRolls::Int->Dice->Dice
addRolls n (Dice dicePos diceRolls) = Dice (norm 100 (dicePos+n)) (diceRolls+n)

playerMove::Int -> Player -> Player
playerMove n (Player pos score) = Player newPos (score+newPos) where
    newPos = norm 10 (pos+n)

norm::Int->Int->Int
norm m v = 1+(v-1)`mod`m