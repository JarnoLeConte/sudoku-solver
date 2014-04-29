{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import qualified Data.Map as M
import Data.List
import Data.Char
import System.IO
import Text.PrettyPrint



-- WINDOWS ONLY:
--import Data.Char
--import Control.Monad (liftM, forever)
--import Foreign.C.Types
--getHiddenChar = liftM (chr.fromEnum) c_getch
--foreign import ccall unsafe "conio.h getch"
--  c_getch :: IO CInt

-- OTHERWISE:
getHiddenChar = getChar




type Board = M.Map (Int,Int) [Int]
data Group = Vert | Horz | Cross | Box


main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  interactive (7,2) emptyBoard



-- some boards

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

emptyBoard :: Board
emptyBoard = create (replicate 81 x)

x :: Int
x = 0



-- board logic (automatic solver)

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



        


-- print using pretty print (interactive)

printBoard :: (Int,Int) -> Board -> IO ()
printBoard k = putStrLn . render . docBoard k

docBoard :: (Int,Int) -> Board -> Doc
docBoard a = bottom . vcat . map (vcat . endings . map hcat . transpose) . docRows a
        
docRows :: (Int,Int) -> Board -> [[[Doc]]]
docRows a = map (M.elems . M.mapWithKey (createDoc a)) . rows

rows :: Board -> [Board]
rows b =  [M.filterWithKey (const.(==k).fst) b | k<-[1..9]]

createDoc :: (Int,Int) -> (Int,Int) -> [Int] -> [Doc]
createDoc a k v = map (part v (k == a)) [0,1,2,1]

part :: [Int] -> Bool -> Int -> Doc
part v True 0 = text "+-------"
part v True 1 = text "| ~~~~~ "
part v True 2 = text "| > " <> val v <> text " < "
part v False 0 = text "+-------"
part v False 1 = text "|       "
part v False 2 = text "|   " <> val v <> text "   "

val :: [Int] -> Doc
val [x] = int x
val _   = space

endings :: [Doc] -> [Doc]
endings (x:xs) = x <> char '+' : map (<> char '|') xs

bottom :: Doc -> Doc
bottom d = d $$ text (concat $ replicate 9 "+-------") <> char '+'


-- print compact board

printBoardCompact :: Board -> IO ()
printBoardCompact = putStrLn . showBoardCompact

showBoardCompact :: Board -> String
showBoardCompact = split . concat . map f . M.elems
  where line row  = '|' : intersperse '|' row ++ "|\n"
        split xs = case splitAt 9 xs of 
           (row, []) -> line row
           (row, xs) -> line row ++ split xs
        f [x] = show x
        f xs = " "






-- interactive 

interactive :: (Int,Int) -> Board -> IO ()
interactive k@(i,j) b = do
  putStrLn ""
  printBoard k b
  putStrLn "Commands: [z: solve] [n: new board] [e: example board]"
  putStrLn "          [w: up] [s: down] [a: left] [d: right] [1-9] [x: delete]"
  c <- getHiddenChar
  putStrLn $ show $ ord c
  let exec c | c `elem` ['w','s','a','d'] = navigate c k b
             | c `elem` ['1'..'9']        = fillIn c k b
             | c == 'x' || ord c `elem` [126,127] = fillIn 'x' k b
             | c == 'z'                   = interactive k (solve b)
             | c == 'n'                   = interactive k emptyBoard
             | c == 'e'                   = interactive k (if b == board1 then board2 else board1)
             | otherwise                  = interactive k b
  exec c

navigate :: Char -> (Int,Int) -> Board -> IO ()
navigate c (i,j) b = case c of
    'w' -> next (i-1,j) --up
    's' -> next (i+1,j) --down
    'd' -> next (i,j+1) --right
    'a' -> next (i,j-1) --left
  where next (k,l) | 1<=k&&k<=9 && 1<=l&&l<=9 = interactive (k,l) b
                   | otherwise = interactive (i,j) b

fillIn :: Char -> (Int,Int) -> Board -> IO ()
fillIn c k b = interactive k $ M.adjust (const $ value c) k b
  where value c | c == 'x'  = [1..9]
                | otherwise = [digitToInt c]


