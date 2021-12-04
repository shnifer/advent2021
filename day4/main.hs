import Data.List.Split
import Data.List

type Board = [[Int]]

main::IO ()
main = do
    raw <- lines <$> readFile "input.txt"
    let (moves, boards) = parseRaw raw
    print $ solve1 moves boards
    print $ solve2 moves boards

solve1:: [Int] -> [Board] -> Int
solve1 moves boards = let
    (t, b) = findWinner moves boards 0
    s = sumBoard b
    in moves!!t * s

solve2:: [Int] -> [Board] -> Int
solve2 moves boards = let
    (t, b) = findLastWinner moves boards 0
    s = sumBoard b
    in moves!!t * s

findWinner:: [Int] -> [Board] -> Int -> (Int, Board)
findWinner moves boards turn = let
    n = head moves
    newBoards = map (remove n) boards
    winner = find isWin boards
    in case winner of
        Just board -> (turn-1, board)
        Nothing -> findWinner (tail moves) newBoards (turn+1)

findLastWinner:: [Int] -> [Board] -> Int -> (Int, Board)
findLastWinner moves boards turn = let
    n = head moves
    newBoards = map (remove n) boards
    stillLosing = filter (not.isWin) newBoards
    in case stillLosing of
        [] -> (turn, head newBoards)
        otherwise -> findLastWinner (tail moves) stillLosing (turn+1)

sumBoard::Board -> Int
sumBoard = sum .map (sum.map(max 0))

remove::Int->Board->Board
remove n = map (map (\x -> if x==n then (-1) else x))

isWin::Board -> Bool
isWin board = empty `elem` board || empty `elem` transpose board
    where empty = [(-1),(-1),(-1),(-1),(-1)]

parseRaw::[String] -> ([Int], [Board])
parseRaw raw = let
    firstStr = head raw
    moves = map read $ splitOn "," firstStr :: [Int]
    boardsStrs = tail $ splitOn [""] raw
    boards = map (map (map read.chunksOf 3)) boardsStrs :: [Board]
    in (moves,boards)