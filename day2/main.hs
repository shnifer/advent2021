import Data.List.Split

main::IO ()
main = do
    raw <- readFile "input.txt"
    let cmds = map parseCmd.lines $ raw
    print $ solve1 cmds
    print $ solve2 cmds

solve1::[Cmd]->Int
solve1 cmds = x*y where (x,y) = foldl applyCmd (0,0) cmds

solve2::[Cmd]->Int
solve2 cmds = x*y where (x,y,a) = foldl applyCmd2 (0,0,0) cmds

applyCmd::(Int,Int) -> Cmd -> (Int, Int)
applyCmd (x,y) (Forward v) = (x+v, y)
applyCmd (x,y) (Down v) = (x, y+v)
applyCmd (x,y) (Up v) = (x, y-v)

applyCmd2::(Int, Int, Int) -> Cmd -> (Int, Int, Int)
applyCmd2 (x,y,a) (Forward v) = (x+v, y+v*a, a)
applyCmd2 (x,y,a) (Down v) = (x, y, a+v)
applyCmd2 (x,y,a) (Up v) = (x, y, a-v)


data Cmd = Forward Int | Down Int | Up Int deriving (Eq, Show)

parseCmd::String -> Cmd
parseCmd str = let
    parts = splitOn " " str
    cmds = parts!!0
    val = read (parts!!1) :: Int
    in case cmds of
        "forward" -> Forward val
        "down" -> Down val
        "up" -> Up val
