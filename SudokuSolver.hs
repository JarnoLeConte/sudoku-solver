module SudokuSolver where
import qualified Data.Map as M
import Data.List

type Board = M.Map (Int,Int) [Int]
data Group = Vert | Horz | Cross | Box

main :: IO ()
main = do
  putStrLn "Board 1 solved:"
  printBoard $ solve $ board1
  putStrLn "Board 2 solved:"
  printBoard $ solve $ board2

create :: [Int] -> Board
create = M.fromList . zip [(i,j)|i<-[1..9],j<-[1..9]] . map f
  where f 0 = [1..9]
        f v = [v]

solve :: Board -> Board
solve b | elim b == b = b              -- complete or fix point
          | otherwise = solve (elim b) -- recursive solving

elim :: Board -> Board 
elim b = foldr elim' b $ groups [(Horz,[1..9]),(Vert,[1..9]),(Cross,[1,2]),(Box,[1..9])]
  where groups = concat . map (\(g,ns) -> map (getGroup g) ns)
        elim' group b = M.union (elimUniq . elimFinals $ group b) b

elimFinals :: Board -> Board
elimFinals b = M.union finals others'
  where (finals, others) = M.partition ((==1).length) b
        others' = M.map (flip (\\) $ concat $ M.elems finals) others

elimUniq :: Board -> Board
elimUniq b = M.map setUniq b
  where uniqs = concat $ filter ((==1).length) $ group $ sort $ concat $ M.elems b
        setUniq xs = case intersect uniqs xs of
                       [x] -> [x]
                       _ -> xs

getGroup :: Group -> Int -> Board -> Board
getGroup g n = M.filterWithKey (inGroup g)
  where inGroup Horz (i,_) _  = i == n
        inGroup Vert (_,j) _  = j == n 
        inGroup Cross (i,j) _ = if n==1 then i==j else i==10-j
        inGroup Box (i,j) _   = let (x,y) = [(k,l)|l<-[1,4,7],k<-[1,4,7]] !! (n-1)
                                in (x<=i&&i<=x+2) && (y<=j&&j<=y+2) 



-- utils

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

showBoard :: Board -> String
showBoard = split . concat . map f . M.elems
  where line row  = '|' : intersperse '|' row ++ "|\n"
        split xs = case splitAt 9 xs of 
           (row, []) -> line row
           (row, xs) -> line row ++ split xs
        f [x] = show x
        f xs = " "
        


-- some boards

x :: Int
x = 0

board1 :: Board
board1 = create
  [x,5,6,2,x,x,7,x,x
  ,1,x,x,x,x,x,x,x,x
  ,7,3,x,x,x,x,x,x,2
  ,x,x,x,x,3,x,6,x,x
  ,x,x,x,x,x,9,x,2,4
  ,x,x,1,x,x,x,x,x,3
  ,x,6,x,x,x,x,x,8,x
  ,3,x,x,x,x,x,x,x,6
  ,x,8,x,x,1,x,5,x,x]


board2 :: Board
board2 = create
  [x,x,x,6,x,x,x,x,x
  ,x,x,5,x,3,1,x,x,4
  ,8,2,x,4,x,x,x,x,x
  ,5,x,x,x,x,x,x,2,7
  ,x,4,x,1,x,x,x,9,x
  ,x,6,x,x,x,x,x,x,x
  ,6,x,x,3,x,x,x,x,9
  ,x,x,4,8,x,x,x,x,x
  ,x,x,8,5,x,x,x,x,x]



