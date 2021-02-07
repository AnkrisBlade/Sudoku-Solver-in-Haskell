{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text


import qualified Data.List.Split as S
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M (toList, fromListWith)

import System.Directory

class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a


instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

type Posibles = V.Vector Int
data Cell = Fixed Int | Posible Posibles  deriving (Show, Eq)
type Sudoku = A.Array (Int, Int) Int
type Grid = A.Array (Int, Int) Cell

ejsudoku = "9 2 0 8 0 0 0 4 0\n0 0 1 3 4 0 0 0 6 \n4 0 3 0 5 6 8 0 0 \n2 1 0 0 6 4 5 0 0 \n0 0 4 5 0 0 0 0 0 \n6 0 5 1 0 2 4 7 0 \n0 0 8 0 2 0 0 1 3 \n0 4 0 0 1 0 0 0 0 \n0 9 7 6 0 3 2 0 4"
ejsudoku2 = "0 5 0 0 6 0 0 0 1 \n0 0 4 8 0 0 0 7 0 \n8 0 0 0 0 0 0 5 2 \n2 0 0 0 5 7 0 3 0 \n0 0 0 0 0 0 0 0 0 \n0 3 0 6 9 0 0 0 5 \n7 9 0 0 0 0 0 0 8 \n0 1 0 0 0 6 5 0 0 \n5 0 0 0 3 0 0 6 0"
ejsudoku3 = "0 0 0 0 6 0 0 8 0 \n0 2 0 0 0 0 0 0 0 \n0 0 1 0 0 0 0 0 0 \n0 7 0 0 0 0 1 0 2 \n5 0 0 0 3 0 0 0 0 \n0 0 0 0 0 0 4 0 0 \n0 0 4 2 0 1 0 0 0 \n3 0 0 7 0 0 6 0 0 \n0 0 0 0 0 0 0 5 0"

cargaSudoku :: String -> Maybe Sudoku
cargaSudoku text
    | L.length values == 81 = Just $ A.listArray ((1,1), (9,9)) ts
    | otherwise = Nothing
    where 
        ts = L.map read values
        values = L.filter (`L.elem` ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])  (L.words text)

posibilidades :: Int -> Int -> Sudoku -> Posibles 
posibilidades f c a 
  | (A.!) a (f,c) /= 0 = V.fromList [(A.!) a (f,c)]
  | otherwise = V.fromList (L.filter ((flip L.notElem) xs) [1..9])
    where xs = [(A.!) a (i,c) | i <- [1..9], (A.!) a (i,c) /= 0] L.++
                [(A.!) a (f,j) | j <- [1..9], (A.!) a (f,j) /= 0] L.++ 
                [(A.!) a (i,j) | i <- [3*fs+1..3*fs+3], j <- [3*cs+1..3*cs+3], (A.!) a (i,j)/=0]
          fs = div (f-1) 3
          cs = div (c-1) 3

cargaCell :: Sudoku -> [Cell]
cargaCell sudoku = 
    getCell (A.assocs sudoku)
    where
        getCell [] = []
        getCell (((i,j), v):vs)
            | V.length ps == 0 = []
            | V.length ps == 1 = Fixed (V.head ps):getCell vs
            | otherwise = Posible ps:getCell vs
                where
                    ps = posibilidades i j sudoku

cargaGrid :: [Cell] -> Maybe Grid
cargaGrid c = case c of
    cs 
        | L.length cs /= 81 -> Nothing
        | otherwise -> Just $ A.listArray ((1,1), (9,9)) cs

minchoice :: Grid -> ((Int, Int), Posibles)
minchoice g = minchoice' [((i,j),x) | ((i,j), Posible x) <- A.assocs g]

minchoice' :: [((Int, Int), Posibles)] -> ((Int, Int), Posibles)
minchoice' (((i,j),v):gs) = L.foldr accum ((i,j), v) (((i,j),v):gs)
    where 
        accum (p, xs) r@((i,j),v) =
            if V.length xs < V.length v 
            then (p, xs)
            else r
 
siguienteGrid :: Int -> Int -> Int -> Grid -> Sudoku
siguienteGrid f c v g = 
  A.array (A.bounds g) [((i,j), func i j) | i <- [1..9], j <- [1..9]]
  where 
    func i j
      | i == f && j == c = v
      | otherwise = cellvalue ((A.!) g (i,j))

siguienteGrid' :: ((Int, Int), Posibles) -> Grid -> [Sudoku]
siguienteGrid' ((f,c), vs) g  
    | V.null vs = []
    | otherwise = siguienteGrid f c (V.head vs) g:siguienteGrid' ((f,c), V.tail vs) g
 
cellvalue :: Cell -> Int
cellvalue (Fixed l) = l
cellvalue _ = 0

isGridSolved :: Grid -> Bool
isGridSolved s = L.null [x | Posible x <- A.elems s]

isInvalidGrid :: Grid -> Bool
isInvalidGrid s = L.any (\y -> snd y > 9) (M.toList (M.fromListWith (+) [(x, 1) | Fixed x <- A.elems s]))

solve :: Sudoku -> Maybe Grid
solve sudoku = cargaGrid (cargaCell sudoku) >>= solve'
    where
        solve' g
            | isInvalidGrid g = Nothing
            | isGridSolved g = Just g
            | otherwise =
                let xs = siguienteGrid' (minchoice g) g
                in L.foldr ((<|>) . solve) Nothing xs

showGrid :: Grid -> [[Int]]
showGrid g = S.chunksOf 9 [x | Fixed x <- A.elems g]

showSudoku :: Sudoku -> [[Int]]
showSudoku s = S.chunksOf 9 [x | x <- A.elems s]


-- Funciones para la ilustracíon del grid y de los valores
todo :: [[Int]] -> Picture
todo cs = pictures [grid, cells (cs)]

-- cells: a partir de los valores genera los textos a ilustrar
cells :: [[Int]] -> Picture
cells vs = translated (-4.5) (5.5) (textop (vs!!0!!0) (vs!!0!!1) (vs!!0!!2)) <>
  translated (0) (5.5) (textop (vs!!0!!3) (vs!!0!!4) (vs!!0!!5)) <>
  translated (4.5) (5.5) (textop (vs!!0!!6) (vs!!0!!7) (vs!!0!!8)) <>
  translated (-4.5) (4.5) (textop (vs!!1!!0) (vs!!1!!1) (vs!!1!!2)) <>
  translated (0) (4.5) (textop (vs!!1!!3) (vs!!1!!4) (vs!!1!!5)) <>
  translated (4.5) (4.5) (textop (vs!!1!!6) (vs!!1!!7) (vs!!1!!8)) <>
  translated (-4.5) (3.5) (textop (vs!!2!!0) (vs!!2!!1) (vs!!2!!2)) <>
  translated (0) (3.5) (textop (vs!!2!!3) (vs!!2!!4) (vs!!2!!5)) <>
  translated (4.5) (3.5) (textop (vs!!2!!6) (vs!!2!!7) (vs!!2!!8)) <>
  translated (-4.5) (1) (textop (vs!!3!!0) (vs!!3!!1) (vs!!3!!2)) <>
  translated (0) (1) (textop (vs!!3!!3) (vs!!3!!4) (vs!!3!!5)) <>
  translated (4.5) (1) (textop (vs!!3!!6) (vs!!3!!7) (vs!!3!!8)) <>
  translated (-4.5) (0) (textop (vs!!4!!0) (vs!!4!!1) (vs!!4!!2)) <>
  translated (0) (0) (textop (vs!!4!!3) (vs!!4!!4) (vs!!4!!5)) <>
  translated (4.5) (0) (textop (vs!!4!!6) (vs!!4!!7) (vs!!4!!8)) <>
  translated (-4.5) (-1) (textop (vs!!5!!0) (vs!!5!!1) (vs!!5!!2)) <>
  translated (0) (-1) (textop (vs!!5!!3) (vs!!5!!4) (vs!!5!!5)) <>
  translated (4.5) (-1) (textop (vs!!5!!6) (vs!!5!!7) (vs!!5!!8)) <>
  translated (-4.5) (-3.5) (textop (vs!!6!!0) (vs!!6!!1) (vs!!6!!2)) <>
  translated (0) (-3.5) (textop (vs!!6!!3) (vs!!6!!4) (vs!!6!!5)) <>
  translated (4.5) (-3.5) (textop (vs!!6!!6) (vs!!6!!7) (vs!!6!!8)) <>
  translated (-4.5) (-4.5) (textop (vs!!7!!0) (vs!!7!!1) (vs!!7!!2)) <>
  translated (0) (-4.5) (textop (vs!!7!!3) (vs!!7!!4) (vs!!7!!5)) <>
  translated (4.5) (-4.5) (textop (vs!!7!!6) (vs!!7!!7) (vs!!7!!8)) <>
  translated (-4.5) (-5.5) (textop (vs!!8!!0) (vs!!8!!1) (vs!!8!!2)) <>
  translated (0) (-5.5) (textop (vs!!8!!3) (vs!!8!!4) (vs!!8!!5)) <>
  translated (4.5) (-5.5) (textop (vs!!8!!6) (vs!!8!!7) (vs!!8!!8))

-- grid: genera el grid
grid :: Picture
grid = translated (-4.5) (4.5) rectangulo <>
  translated (0) (4.5) rectangulo <>
  translated (4.5) (4.5) rectangulo <>
  translated (-4.5) (0) rectangulo <>
  translated (0) (0) rectangulo <>
  translated (4.5) (0) rectangulo <>
  translated (-4.5) (-4.5) rectangulo <>
  translated (0) (-4.5) rectangulo <>
  translated (4.5) (-4.5) rectangulo 

-- rectangulo: cuadrados del grid
rectangulo :: Picture
rectangulo = rectangle 4.5 4.5

-- cell: transforma String a Text
cell :: String -> String -> String -> Text
cell i c d = pack (i++"   "++c++"   "++d)

-- textop: usando cell, generamos una imagen conteniendo 3 valores
textop :: Int -> Int -> Int -> Picture
textop i c d = lettering l 
  where l = (cell (show i) (show c) (show d))
  
-- main: elegimos el sudoku a resolver y dibujar manualmente,
-- se podría automatizar.
main :: IO ()
main = do
    let Just sudoku = cargaSudoku ejsudoku -- o "ejsudoku2, ejsudoku3"
    let res3 = showSudoku sudoku
    let Just res = solve sudoku
    let res2 = showGrid res
    drawingOf (translated 10 0 (todo res2) <> translated (-10) 0 (todo res3))
