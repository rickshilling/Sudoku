module Sudoku
    (
      solve,
      rowComplete,
      complete,
      mm,
      blocked,
      blockedRow,
      blockedEntry,
      search,
      expand,
      expandHelper,
      expandRow,
      p,
      prune,
      pruneRow,
      pruneRowHelper,
      pruneEntries,
      pruneEntry,
      ee,
      m,
      r,
      s,
      Row,
      Matrix,
      Grid,
      collapse,
      collapseRow,
      rowChoice,
      choices,
      candidates,
      valid,
      nodups,
      getCols,
      getBox,
      getRows,
      getRow,
      boxToColIndices,
      boxToRowIndices,
      boxs,
      cols,
      rows,
      empty,
      easy
    )
  where

import Data.List
import Control.Monad

type Grid      =  Matrix Char
type Matrix a  =  [Row a]
type Row a     =  [a]

empty :: Grid
empty = replicate 9 (replicate 9 ' ')

candidates = "123456789"

easy  :: Grid
easy   = [
  " 5  6   1",
  "  48   7 ",
  "8      52",
  "2   57 3 ",
  "         ",
  " 3 69   5",
  "79      8",
  " 1   65  ",
  "5   3  6 "]

r = [[1,2],[3,4,5]] :: Row [Int]
s = [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]] :: Row [Int]
m = [["12","3"],["4","12"]]
p = [["1","124"],["13","34"]]
ee = choices easy
mm = [["1","12","1"],["34","","3"]]

solve :: Grid -> [Grid]
solve  = search . prune . choices

search :: Matrix [Char] -> [Grid]
search m
  | blocked m  = []
  | complete m = collapse m
  | otherwise  = [g | m' <- expand m
                    , g  <- search (prune m')]

complete :: Matrix [Char] -> Bool
complete m = completeHelper (rows m) && completeHelper (cols m) && completeHelper (boxs m)
  where
    completeHelper []     = True
    completeHelper (x:[]) = rowComplete x
    completeHelper (x:xs) = rowComplete x && completeHelper xs

rowComplete :: Row [Char] -> Bool
rowComplete xs = rowCompleteHelper xs "123456789"
  where rowCompleteHelper xs     [] = True
        rowCompleteHelper []     ts = (length ts == 0)
        rowCompleteHelper (x:xs) ts =
          if length x /= 1 then
            False
          else
            (elem (x !! 0) ts ) && (rowCompleteHelper xs (ts\\x))

blocked :: Matrix [Char] -> Bool
blocked []     = False
blocked (x:xs) = blockedRow x || blocked xs

blockedRow :: Row [Char] -> Bool
blockedRow xs = blockedRowHelper [] xs
  where
    blockedRowHelper _ [] = False
    blockedRowHelper ls (r:rs) = (any (|| False) $ map (blockedEntry r) (ls ++ rs)) || (blockedRowHelper (ls++[r]) (rs))

blockedEntry :: [Char] -> [Char] -> Bool
blockedEntry _  [] = True
blockedEntry [] _  = True
blockedEntry x y =
  if not (length x == 1 && length y == 1) then
    False
  else
    (x !! 0) == (y !! 0)

expand :: Matrix [a] -> [Matrix [a]]
expand = fst . expandHelper

expandHelper :: Matrix [a] -> ([Matrix [a]],Bool)
expandHelper []      = ([],False)
expandHelper (xs:[]) = ([[z] | z <- result_row],result_flag)
  where (result_row,result_flag) = expandRow xs
expandHelper (xs:ys) =
  case expandRow xs of
    (xs_result_row, False) -> ([ [xs] ++ zs | zs <- ys_result_matrix ], ys_result_flag)
    (xs_result_row, True)  -> ([ [x]  ++ ys | x  <- xs_result_row] ,True)
    where
      (ys_result_matrix,ys_result_flag) = expandHelper ys

expandRow :: Row [a] -> ([Row [a]],Bool)
expandRow [] = ([],False)
expandRow (xs:[]) = ([[[x]] | x <- xs ], length xs > 1)
expandRow (xs:ys) =
    case length xs of
      1 -> ([ [xs] ++zs | zs <- result_row],result_flag)
        where
          (result_row,result_flag) = expandRow ys
      _ -> ([ [[x]]++ys | x <- xs ], True)

prune :: Matrix [Char] -> Matrix [Char]
prune m = fixed_m
  where
    result_1 = rows $ (map pruneRow (rows $ m))
    result_2 = cols $ (map pruneRow (cols $ result_1))
    result_3 = boxs $ (map pruneRow (boxs $ result_2))
    fixed_m =
      if m == result_3 then
        m
      else
        prune result_3

pruneRow :: Row [Char] -> Row [Char]
pruneRow xs = fixed_result
  where
    result = pruneRowHelper [] xs
    list_result = fst result
    bool_result = snd result
    fixed_result =
      if bool_result == True then
        pruneRow list_result
      else
        list_result

pruneRowHelper :: [[Char]] -> [[Char]] -> ([[Char]],Bool)
pruneRowHelper ls []     = (ls,False)
pruneRowHelper ls (r:rs) = (result_list, result_bool)
      where
        l_pair = pruneEntries r ls
        r_pair = pruneEntries r rs
        l_bool = any (|| False) $ map snd l_pair
        r_bool = any (|| False) $ map snd r_pair
        l_list = map fst l_pair
        r_list = map fst r_pair
        result_pair = pruneRowHelper (l_list++[r]) r_list
        result_list = fst result_pair
        result_bool = (snd result_pair) || l_bool || r_bool

pruneEntries :: [Char] -> [[Char]] -> [([Char],Bool)]
pruneEntries x ys = map (pruneEntry x) ys 

pruneEntry :: [Char] -> [Char] -> ([Char],Bool)
pruneEntry a b =
  if length a == 1 && length_b /= 1 then
    (result, pruneFlag)
  else
    (b,False)
  where
    result    = b \\ a
    pruneFlag = length_b /= length result
    length_b  = length b

collapse :: Matrix [a] -> [Matrix a]
collapse [] = []
collapse (rs:[])   = [[r]      | r <- collapseRow rs]
collapse (rs:ms)   = [[r] ++ m | r <- collapseRow rs, m <- collapse ms]

collapseRow :: Row [a] -> [Row a]
collapseRow [] = [[]]
collapseRow (xs:[]) = [[x]      | x <- xs ]
collapseRow (xs:ys) = [[x] ++ y | x <- xs, y <- collapseRow ys]

choices :: Grid -> Matrix [Char]
choices g = map rowChoice g

rowChoice :: Row Char -> Row [Char]
rowChoice [] = []
rowChoice (x:xs)
  | x == ' '  = [candidates] ++ (rowChoice xs)
  | otherwise = [[x]] ++ (rowChoice xs)

valid :: Grid -> Bool
valid g = (nodups $ rows g) && (nodups $ cols g) && (nodups $ boxs g)

nodups :: Eq a => [Row a] -> Bool
nodups [] = True
nodups (x:xs) = ((length $ nub x) == length x) && nodups xs

rows :: Matrix a -> [Row a]
rows m = m

cols :: Matrix a -> [Row a]
cols m = transpose m

boxs :: Matrix a -> [Row a]
boxs m = map (getBox m) [0..((length m) -1)]

getBox :: Matrix a -> Int -> Row a
getBox m boxIndex = getCols (map (m!!) rowIndices) colIndices
  where
     rowIndices = map fst boxIndices
     colIndices = map snd boxIndices
     boxIndices = boxToIndices boxIndex

getRows :: Matrix a -> [Int] -> [Row a]
getRows m rowIndices = map (getRow m) rowIndices

getRow :: Matrix a -> Int -> Row a
getRow (x:xs) n
  | n == 1 = x
  | otherwise = getRow xs (n-1)

getCols :: [Row a] -> [Int] -> Row a
getCols [] [] = []
getCols (r:rs) (n:ns) = (r!!n):(getCols rs ns)

boxToIndices :: Int -> [(Int,Int)]
boxToIndices boxIndex = [(i,j)|i<-boxToRowIndices boxIndex, j<-boxToColIndices boxIndex]

boxToRowIndices :: Int -> [Int]
boxToRowIndices n
  | 0<=n && n<=2 = [0,1,2]
  | 3<=n && n<=5 = [3,4,5]
  | otherwise    = [6,7,8]

boxToColIndices :: Int -> [Int]
boxToColIndices n
  | n==0 || n==3 || n==6 = [0,1,2]
  | n==1 || n==4 || n==7 = [3,4,5]
  | otherwise            = [6,7,8]
